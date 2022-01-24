sap.ui.define([
	"sap/ui/base/Object"
], function (object) {
	"use strict";

	/**
	 * With this class you can now register your data pattern and check if they are valid or not.
	 **/
	return object.extend("MAT_ASSET_IN.ZASSET_IN.zassetin.model.Classes.DataVerifier", {

		constructor: function (oService, schemaNamespace, model = 'YOUR MAIN MODEL HERE OR LET IT BE DYNAMIC') {
			this._oDataStructure = this.generateControleObject(oService, schemaNamespace)[model];
			this._errorMessage = {
				'String': {
					'nullable': "Ce champ ne peut pas être vide.",
					'maxLength': "Taille maximum dépassé ! ({X})."
				},
				'DateTime': {
					'precision': "Mauvais format."
				},
				'Decimal': {
					'nullable': "Ce champ ne peut pas être vide.",
					'precision': "Mauvais format. (precision)",
					'scale': "Mauvais format. (scale)"
				}
			};
		},

		/**
		 * Verify the input object is valid
		 * @public
		 * @params {object} obj - the object you want to verify
		 * @params {bool} isStrict - accept to have more property then what's defined in oDataStructure.
		 * @return {[bool, object]} - error message object
		 **/
		verify: function (obj = {}, isStrict = false) {
			if (!(typeof(obj) == 'object'))
				throw 'Parameter is not a dictionary !';
			
			let errorStatus = {error: false}; /** Pass by reference **/
			let errorObject = this.recDeepStruct(obj, this._oDataStructure, errorStatus);
			
			console.log("Verify finished for object :", obj, errorStatus, errorObject);
			return { error: errorStatus.error, response: errorObject };
		},
		
		/**
		 * Go to to the tail of each branch to find the variable and match it with the control.
		 * @private
		 * @params {Dict or variable} dataStruct
		 * @params {Dict} controlStruct
		 * @params {{}} response
		 * @return [string]
		 **/
		recDeepStruct: function(dataStruct, controlStruct, errorStatus, response = {}) {
			if (controlStruct == undefined)
				/**  No metadata for this property **/
				return;

			if (dataStruct == undefined || typeof(dataStruct) != 'object') {
				/** If given dataStruct is not 'object' means it's not a dictionary and then it must be a value **/
				/** So it's useless to go througt the loop of key. **/
				return this.verifyValueByProperties(dataStruct, controlStruct);
			}

			for (const [key, value] of Object.entries(dataStruct))
				if (key != '__metadata') {
					response[key] = this.recDeepStruct(dataStruct[key], controlStruct[key], errorStatus);
					errorStatus.error = errorStatus.error ? errorStatus.error : response[key]?.length > 0;
				}

			return response;
		},

		/**
		 * Redirect verifying to correct verify method
		 * @public
		 * @params {object} value - your value
		 * @params {dict} properties - properties dict
		 * @return {string} - error message
		 **/
		verifyValueByProperties: function (value, properties) {
			let classContext = this,
				typeToMethod = {
				'String': {method: this.verifyString.bind(classContext)},
				'Decimal': {method: this.verifyInt.bind(classContext)},
				'DateTime': {method: this.verifyDate.bind(classContext)}
			}
			return typeToMethod[properties.type].method(value, properties);
		},

		/**
		 * Verify value string by properties
		 * @public
		 * @params {string} value
		 * @params {dict} properties
		 * @return {string} - error message
		 **/
		verifyString: function (value, properties) {
			if (value == undefined || value == '')
				return (properties.nullable == "false") ? this._errorMessage['String']['nullable'] : '';
			if (properties.maxLength != 0 && properties.maxLength != undefined &&
				value.length > (+properties.maxLength))
				return this._errorMessage['String']['maxLength'].replace('{X}', properties.maxLength);
			return "";
				
		},

		/**
		 * Verify value int by properties
		 * @public
		 * @params {int} value
		 * @params {dict} properties
		 * @return {string} - error message
		 **/
		verifyInt: function (value, properties) {
			if (value == undefined || value == '')
				return properties.nullable == "false" ? this._errorMessage['Decimal']['nullable'] : '';
			return "";
			// Precision
			// Scale
		},
		/**
		 * Verify value DateTime by properties
		 * @public
		 * @params {DateTime} value
		 * @params {dict} properties
		 * @return {string} - error message
		 **/
		verifyDate: function (value, properties) {
			// if (value == undefined || value == '')
			// 	return properties.nullable == "false" ? this._errorMessage['DateTime']['nullable'] : '';
			// Precision
			return "";
		},

		/**
		 * Generate a control object based on the odata metadata
		 * @private
		 * @params {oData} oService - the odata object
		 * @return {object} - the contol object
		 **/
		generateControleObject: function (oService, namespace) {
			let oMetadata = oService.getServiceMetadata()?.dataServices?.schema?.find((x) => x.namespace = namespace),
				_complexTypes = oMetadata?.complexType,
				_entityTypes = oMetadata?.entityType;
			let complexTypes = this.generateDictionaryOfComplexType(_complexTypes),
			    entityTypes = {};
			
			_entityTypes.forEach((x) => {
				entityTypes[x.name] = {};
				x.property.forEach((prop) => {
					let type = prop.type.substring(prop.type.indexOf('.')+1);
					prop.type = type;
					delete prop.extensions;
					entityTypes[x.name][prop.name] = complexTypes[type] ?? prop;
				});
			});

			return entityTypes;
		},

		/**
		 * Generate a dictionnary of complex type
		 * @private
		 * @params {object} complexTypes
		 * @return {dict}
		 **/
		generateDictionaryOfComplexType: function (complexTypes) {
			let dictTypes = {};
			
			// Order to not have complex of complex with complex not being created.
			// Throw error if find cycle.
			complexTypes = this.orderingTypesByDependecy(complexTypes);
			
			complexTypes.forEach((x) => {
				let newStruct = {};

				x.property.forEach((x) => {
					let type = x.type.substring(x.type.indexOf('.')+1);
					if (x.type.substring(0, 4) == 'Edm.') {
						x.type = type;
						delete x.extensions;
						newStruct[x.name] = x;
					} else {
						dictTypes[type].forEach((innerType) => {
							newStruct[innerType.name] = innerType;
						});
					}
				});
				dictTypes[x.name] = newStruct;
			});
			return dictTypes;
		},
		
		/**
		 * Order types to not have a type requiering an
		 * other type which haven't been generated yet.
		 * @private
		 * @params [type] types - all object to sort
		 * @return [type] - ordered list
		 **/
		orderingTypesByDependecy: function(objects) {
			let orderedList = [],
		        saved = {},
		        oldCnt = -1;
			
			while (orderedList.length != objects.length) {
				if (orderedList.length == oldCnt)
					throw 'ERROR: Couldn\'t parse "types" because of a cycle.';

				objects.forEach((obj) => {
					if (!saved[obj.name]) {
						let cntDependency = obj.property.length;
						obj.property.forEach((dep) => { 
							// Check if all dependency exists
							let type = dep.type;
							if (type.substring(0,4) == "Edm." || saved[type.substring(type.indexOf('.')+1)])
							    --cntDependency;
						});
						// if they exist, save this type.
						if (cntDependency == 0) {
							saved[obj.name] = obj;
							orderedList.push(obj);
						}
					}
				});
				oldCnt = orderedList.length;
			}
			return orderedList;
		}

	})
});

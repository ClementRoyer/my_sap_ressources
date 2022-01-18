# Initialize and create an edit dialog.

``` JS
// "sap/ui/core/Fragment"

/**
 * Utils method, made to clarify code and set models more easly.
 * @protected
 * @param {sap.ui.json.Model.JSONModel} oModel - data you want to set to the new model.
 * @param {object} sModel - the name you want to give to the new model (can be undefined or empty to target root model).
 **/
setModel: function (oModel, sModel) {
  if (oModel instanceof JSONModel == false)
    oModel = new JSONModel(oModel);
  if (!sModel)
    this.getView().setModel(oModel);
  else
    this.getView().setModel(oModel, sModel);
},


/**
 * Initialize and show the create new asset dialog.
 * @public
 * @params {object} object - The object you want to open
 **/
showUpdateDialog: function(object) {
    let fragmentURL = '',
        // Deep copy of the initial object
        // this way it won't update the real object unless we tell it to.
        objectCopy = Object.assign({}, object),
        objectCopyContext = {};
     
     // Set or reset edit model with current object copy.
     this.setModel(objectCopy, "object");
     objectCopyContext = new sap.ui.model.Context(this.getModel("object"));
     
     if (!this._updateDialog) {
         // First load of the fragment
         Fragment.load({
             name: fragmentURL,
             containingView: this.getView(),
             controller: this
         }).then( function(dialog) {
             dialog.setBindingContext(objectCopyContext, "object");
             this._updateDialog = dialog;
             this.getView().addDependent(this._updateDialog);
             this._updateDialog.open();
         }.bind(this));
     } else {
         // Fragment already exist, refresh its binding.
         this._updateDialog.setBindingContext(objectCopyContext, "object");
         this._updateDialog.open();
     } 
},
```

> Use like `object>/xxxx` in xml fragment

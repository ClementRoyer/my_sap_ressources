# Using Fiori Theme LESS Variables in Custom CSS

> This guide explains how to use LESS-based CSS variables from your SAP Fiori theme in your custom styles. \

## Enable `data-sap-ui-xx-cssVariables`

To access theme variables as CSS custom properties (`--sap*`) in your application, you must enable the `data-sap-ui-xx-cssVariables` setting. You can do this in one of two ways:

### Option 1: Via `index.html`

Add the `data-sap-ui-xx-cssVariables="true"` attribute to your UI5 bootstrap script tag:

```html
<!-- index.html -->
<script
  id="sap-ui-bootstrap"
  src="resources/sap-ui-core.js"
  ...
  data-sap-ui-xx-cssVariables="true" <!-- Enables CSS Variables -->
  ...>
</script>
```

### Option 2: As a URL Parameter

You can also enable it via a URL parameter when launching your application:

``` URL
{your_system_url}/fiori?sap-ui-xx-cssVariables=true
```

### Verify Activation

To confirm that the CSS variables are available, open the browser console and run:

```javascript
getComputedStyle(document.documentElement).getPropertyValue('--sapBrandColor')
```

You can also inspect the `:root` selector in the browser's developer tools to see all available CSS variables.

## Using Theme Variables in Your CSS

Once enabled, you can use the variables in your custom CSS files like this:

```css
.myCustomComponent {
  background-color: var(--sapBrandColor);
  color: var(--sapTextColor);
}
```

> If you are working in SAP Business Application Studio (BAS) and using a custom Fiori theme, [follow this SAP tutorial](https://community.sap.com/t5/technology-blog-posts-by-members/use-custom-fiori-theme-during-bas-preview/ba-p/13878912) to ensure your custom theme is loaded during preview.


##

Sources:
* https://community.sap.com/t5/technology-blog-posts-by-sap/ui5ers-buzz-52-the-rendering-evolution-semantic-rendering-and-css-variables/ba-p/13446742]

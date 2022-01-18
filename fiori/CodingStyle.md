# Coding style

In this file I will list all rules that I found or etabilshed for the developments of my FIORI apps.

<!-- TOC -->
- [Naming convention](#naming_convention)

## Naming convention

> Source: [ktaranov/naming-convention](https://github.com/ktaranov/naming-convention/blob/master/JavaScript%20Name%20and%20Coding%20Conventions.md) \
> *Note: This version have been modified to mach my style.*

### JavaScript Coding Conventions

| Object Name        | Notation   | Plural | Prefix | Suffix | Abbreviation | Char Mask  | Underscores |
|--------------------|------------|--------|--------|--------|--------------|------------|-------------|
| Function name      | camelCase  | Yes    | No     | Yes    | Yes          | [A-z][0-9] | No          |
| Function arguments | camelCase  | Yes    | No     | No     | Yes          | [A-z][0-9] | Yes         |
| Local variables    | camelCase  | Yes    | No     | No     | Yes          | [A-z][0-9] | Yes         |
| Constants name     | PascalCase | Yes    | No     | No     | Yes          | [A-z][0-9] | No          |
| Field name         | camelCase  | Yes    | No     | No     | Yes          | [A-z][0-9] | Yes         |


### Coding conventions are style guidelines for programming. They typically cover:

1. Naming and declaration rules for variables and functions.
1. Rules for the use of white space, indentation, and comments.

Coding conventions secure quality:

1. Improves code readability
1. Make code maintenance easier

Always use the same naming convention for all your code. For example:
1. Do use camelCasing for variables, function names and function argument names;
2. Do use PascalCasing for global variables;
3. Do use UPPERCASE for constants (like PI);
4. Do not use under_scores in variable, constants, function arguments or function names;
5. Do not use hyphens in JavaScript names.

#### Naming Conventions

Do use camelCasing for function names.
Do use camelCasing for function arguments and local variables.

*Note: Don't start names with a $ sign. It will put you in conflict with many JavaScript library names.*

### Spaces Around Operators

Always put spaces around operators ( = + / * ), and after commas:

### Code Indentation

Always use 2 spaces for indentation of code blocks:

*Note: Do not use tabs (tabulators) for indentation. Text editors interpret tabs differently.* 

### Statement Rules

General rules for simple statements: Always end simple statement with a semicolon.

#### General rules for complex (compound) statements:

1. Put the opening bracket at the end of the first line.
2. Use one space before the opening bracket.
3. Put the closing bracket on a new line, without leading spaces.
4. Do not end complex statement with a semicolon.

#### Object Rules

General rules for object definitions:

1. Place the opening bracket on the same line as the object name.
2. Use colon plus one space between each property and its value.
3. Use quotes around string values, not around numeric values.
4. Do not add a comma after the last property-value pair.
5. Place the closing bracket, on a new line, without leading spaces.
6. Always end an object definition with a semicolon.

Short objects can be written compressed, on one line.

#### Line Length < 80

For readability, avoid lines longer than 80 characters. If a JavaScript statement does not fit on one line, the best place to break it, is after an operator or a comma.

## Offical Reference

1. [Google JavaScript Style Guide](https://google.github.io/styleguide/jsguide.html#introduction)
2. [JavaScript Style Guide and Coding Conventions](http://www.w3schools.com/js/js_conventions.asp) 


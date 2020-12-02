<!-- TABLE OF CONTENTS -->
## Table of Contents

* [About the project](#about-the-project)
  * [A bit of history](#a-bit-of-history)
  * [Key aspects](#key-aspects)
  * [Building blocks](#building-blocks)
* [IDE](#ide)
  * [Tools](#ide-tools) 
* [Getting Started](#getting-started)
  * [Prerequisites](#prerequisites)
  * [Installation](#installation)
* [Usage](#usage)
* [License](#license)
* [Contact](#contact)

<!-- ABOUT THE PROJECT -->
## About the project
Webside is both the specification of an API to interact with an Smalltalk system (back-end) and a web IDE (front-end) built on top of such API.

The [API](api) could be divided into two conceptually different subsets of endpoints:
* Static: used to browse and change the code
* Dynamic: used to evaluate expression (both synchronically and asynchronically) and retrieve the resulting objects, as well as to debug and profile expressions.

## A bit of history
This project started as a personal training with the motivation of learning React in particular, and improving my JavaScript skills in general.
As a passionate smalltalker and having implemented several web APIs in Smalltalk for differnt purposes and domains, I decided to expose Smalltalk itself thru a web API and see how far I could go implementing classical Smalltalk IDE tools using React.

Being the dialect I use for my daily duties, I started using Bee Smalltalk as the back-end (porting the API to other Smalltalk dialects later). Naturally, the API evolved as I went progressing with the front-end tools, starting by the "static" side (code) and then moving to the "dynamic" side (objects).

## Key aspects
At some point in time, I realized that all the effort should be invested on keeping the API as simple as possible in order to provide these major benefits:
* Enable the implementation in a any Smalltalk system (Bee, Pharo, Squeak, Cuis, Dolphin) without a major effort,
* Make any improvement, change, or even a complete revamping of the IDE, easier.

As an extra lesson derived from the latter, I saw that: 
* Far from being a downside, having a Smalltalk IDE implemented in JavaScript would enable contributions from the JavaScript community to either enhance, change or extend Webside in any direction.
* Not only this IDE could be used to work upon different Smalltalk images but also it could allow several developers to work on the same Smalltalk image at the same time, making Webside a collaborative environment.

### Building blocks
Webside is built using these frameworks and components (plus a set of packages not listed here):
* [ReactJS](https://reactjs.org): the main framework used to build Webside.
* [Material-UI](https://material-ui.com): a set of React useful components with handy interfaces and a coherent style.  
* [CodeMirror](https://codemirror.net/): the component used to display and edit code.

The IDE used for JavaScript was [Visual Studio Code](https://code.visualstudio.com).

## IDE
The global layout of Webside is the following:
  * A title bar including a system search (not implemented yet)
  * A side bar on the left including options to access the Transcript, the system changes browser, chat to start a conversation peers working on the same image and other
  * A side menu (plus button) to open some tools
  * A tab container where the different tools are opened

### IDE tools
As as starting point I decided to recreate the traditional IDE tools of any Smalltalk environment. That is, a class (refactoring) browser, a method browser to list senders/implementors/references, a workspace where to evaluate expressions and the corresponding counterpart, the inspector, to navigate the resulting objects, and the debugger.

Additionally, I included a transcript pane where the user is notified, a system browser to manage some sort of projects, current changes browser, a simple chat, a test runner and a profiler.

In terms of internal design, every tool is implemented by a React component, with common parts such as code editor or method list shared by them.

As it is common to every Smalltalk IDE, every code pane where the programmer can write expressions offers the options to evaluate (and show or inspect the resulting object), debug and profile an expressions, together with a bunch of options to search classes and methods (senders, implementors, etc.) 

Having these tools implemented it is now possible to try new presentation alternatives as some are already explored (look for instance [Glamorous toolkit](https://gtoolkit.com/)).
Again, the key point here is to keep the required API as simple as possible to ease its implementation in a specific Smalltalk system, and then innovate at the presentation layer.


### Class browser
This is more or less the classical refactoring browser, including the hierarchy of classes, their variables (plus some access filters), catetgories and methods at the top, and a method editor at the bottom.

The component implementing this tool is [ClassBrowser](client/src/components/tools/ClassBrowser.js) and it relies in [Code](api/code) and [Changes](api/changes) endpoints to browse and make changes on the code, and in [Evaluations](api/evaluations) and [Objects](api/objects) endpoints to evaluate expressions. 

![Class Browser](docs/images/ClassBrowser.png)


### Method Browser
This is a list of methods resulting from a search (senders, implementors, global references) with the corresponding method editor.

The component implementing this tool is [MethodBrowser](client/src/components/tools/MethodBrowser.js) and as the `ClassBrowser` component it relies in [Code](api/code) and [Changes](api/changes) endpoints to browse and make changes on the code, and in [Evaluations](api/evaluations) and [Objects](api/objects) endpoints to evaluate expressions. 


![Method Browser](docs/images/MethodBrowser.png)


### Workspace
Again a typical pane where the programmer can evaluate expressions (with its own variable scope to keep workspace temporaries) and print or inspect its results. Inspections triggered with the play button are embedded in the same workspace, while those inspections triggered with "Inspect" menu option (or its shortcut) are opened in a different tab.

The component implementing this tool is [Workspace](client/src/components/tools/ClassBrowser.js) and it essentially relies in [Evaluations](api/evaluations) and [Objects](api/objects) endpoints. 

![Workspace](docs/images/Workspace.png)

### Inspector
![Inspector](docs/images/Inspector.png)

### Debugger
![Debugger](docs/images/Debugger.png)

### Test runner
![TestRunner](docs/images/TestRunner.png)

### Profiler
![Profiler](docs/images/Profiler.png)

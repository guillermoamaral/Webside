<!-- TABLE OF CONTENTS -->
## Table of Contents

* [About the project](#about-the-project)
  * [A bit of history](#a-bit-of-history)
  * [Lessons learned](#lessons-learned)
  * [Frameworks](#tools-and-frameworks)
* [Tools](#tools)  
* [Getting Started](#getting-started)
  * [Prerequisites](#prerequisites)
  * [Installation](#installation)
* [Usage](#usage)
* [License](#license)
* [Contact](#contact)

<!-- ABOUT THE PROJECT -->
## About the project
Webside is both the specification of an API to interact with a Smalltalk system (back-end) and a web IDE (front-end) built on top of such API.

The [API](docs/api) could be divided into two conceptually different subsets of endpoints:
* Static: used to browse and change the code (more about the tools)
* Dynamic: used to evaluate expression (both synchronically and asynchronically) and retrieve the resulting object, as well as debug and profile expressions.

## A bit of history
This started as a personal project with the  motivation of learning React in particular, and improving my JavaScript skills in general.
As a passionate smalltalker and having implemented several web APIs in Smalltalk for differnt purposes and domains, I decided to expose Smalltalk itself thru a web API and see how far I could go implementing classical Smalltalk IDE tools using React.

I used Bee Smalltalk as the first back-end (starting to port the API to other Smalltalk dialects later) and made the API evolve as I went progressing with the front-end tools, starting by the "static" side (code) and then adding the "dynamic" side (objects).

## Lessons learned
At some point in time, I realized that all the effort should be invested on keeping the API as simple as possible as this would bring two major benefits:
* It would enable the implementation in a any Smalltalk system (Bee, Pharo, Squeak, Cuis, Dolphin) without a major effort,
* It would make any improvement, change, or even a complete revamping of the IDE easier.

As an extra lesson derived from the latter, I saw that: 
* Far from being a downside, having a Smalltalk IDE implemented in JavaScript would enable contributions from the JavaScript community to either enhance, change or extend Webside in any direction.
* Not only this IDE could be used to work upon different Smalltalk images as its back-end but also it could allow several developers to work on the same Smalltalk system (image) simulataneously, making Webside a collaborative environment.

### Tools and frameworks
Webside is built using these frameworks and components (plus a set of packages not listed here):
* [ReactJS](https://reactjs.org): the main framework used to build Webside.
* [Material-UI](https://material-ui.com): a set of React useful components with handy interfaces and a coherent style.  
* [CodeMirror](https://codemirror.net/): the component used to display and edit code.

The IDE used for JavaScript was [Visual Studio Code](https://code.visualstudio.com)
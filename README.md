<!-- PROJECT LOGO -->
<br />
<p align="center">
  <h3 align="center">Webside</h3>

  <p align="center">
    Smalltalk IDE for the web
    <br />
    <a href="https://github.com/guillermoamaral/Webside"><strong>Explore the docs »</strong></a>
    <br />
    <br />
    <a href="https://github.com/guillermoamaral/Webside">View Demo</a>
    ·
    <a href="https://github.com/guillermoamaral/Webside">Report Bug</a>
    ·
    <a href="https://github.com/guillermoamaral/Webside">Request Feature</a>
  </p>
</p>

<!-- TABLE OF CONTENTS -->
## Table of Contents

* [About the project](#about-the-project)
  * [A bit of history](#a-bit-of-history)
  * [Lessons learned](#lessons-learned)
  * [Tooling](#tooling)
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

The API could be divided into two conceptually different subsets of endpoints:
* Static: the ones that are used to browse and change the code (more about the tools)
* Dynamic: the ones that are used to evaluate, both synchronically and asynchronically, get resulting objects, as well as debug and profile expressions.

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


### Tooling
Webside is built using these frameworks and components (plus a set of packages not listed here):
* [ReactJS](https://reactjs.org): the main framework used to build Webside.
* [Material-UI](https://material-ui.com): a set of React useful components with handy interfaces and a coherent style.  
* [CodeMirror](https://codemirror.net/): the component used to display and edit code.
The IDE used to code JavaScript was [Visual Studio Code](https://code.visualstudio.com)

<!-- GETTING STARTED -->
## Getting Started

### Prerequisites

* npm
```sh
npm install npm@latest -g
```

* Your Smalltalk image implementing the [API](docs/api)

### Installation

1. Clone the repo
```sh
git clone https://github.com/guillermoamaral/Webside.git
```
2. Install NPM packages
```sh
npm install
```

<!-- USAGE EXAMPLES -->
## Usage

Use this space to show useful examples of how a project can be used. Additional screenshots, code examples and demos work well in this space. You may also link to more resources.

_For more examples, please refer to the [Documentation](https://example.com)_


<!-- LICENSE -->
## License

Distributed under the MIT License. See `LICENSE` for more information.



<!-- CONTACT -->
## Contact

Guille Amaral - guillermoamaral@gmail.com

Project Link: [https://github.com/guillermoamaral/Webside](https://github.com/guillermoamaral/Webside)






<!-- MARKDOWN LINKS & IMAGES -->
<!-- https://www.markdownguide.org/basic-syntax/#reference-style-links -->
[contributors-shield]: https://img.shields.io/github/contributors/othneildrew/Best-README-Template.svg?style=flat-square
[contributors-url]: https://github.com/othneildrew/Best-README-Template/graphs/contributors
[forks-shield]: https://img.shields.io/github/forks/othneildrew/Best-README-Template.svg?style=flat-square
[forks-url]: https://github.com/othneildrew/Best-README-Template/network/members
[stars-shield]: https://img.shields.io/github/stars/othneildrew/Best-README-Template.svg?style=flat-square
[stars-url]: https://github.com/othneildrew/Best-README-Template/stargazers
[issues-shield]: https://img.shields.io/github/issues/othneildrew/Best-README-Template.svg?style=flat-square
[issues-url]: https://github.com/othneildrew/Best-README-Template/issues
[license-shield]: https://img.shields.io/github/license/othneildrew/Best-README-Template.svg?style=flat-square
[license-url]: https://github.com/othneildrew/Best-README-Template/blob/master/LICENSE.txt
[linkedin-shield]: https://img.shields.io/badge/-LinkedIn-black.svg?style=flat-square&logo=linkedin&colorB=555
[linkedin-url]: https://linkedin.com/in/othneildrew
[product-screenshot]: images/screenshot.png

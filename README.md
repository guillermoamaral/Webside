<!-- PROJECT LOGO -->
<br />
<p align="center">
  <h3 align="center">Webside</h3>

  <p align="center">
    Smalltalk IDE for the web
    <br />
    <a href="https://github.com/othneildrew/Best-README-Template"><strong>Explore the docs »</strong></a>
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
  * [Built With](#built-with)
* [Getting Started](#getting-started)
  * [Prerequisites](#prerequisites)
  * [Installation](#installation)
* [Usage](#usage)
* [License](#license)
* [Contact](#contact)

<!-- ABOUT THE PROJECT -->
## About the project
This project was born as a motivation to learn React in particular and to improve my JavaScript skills in general.
As long-term (20 years+) passionate smalltalker and having implemented several web APIs (in Smalltalk) in professional contexts, I decided to expose Smalltalk itself thru a web API and see how far I could go implementing some of the Smalltalk IDE tools using React based on such such API. Starting by the "static" side (code) I implementied a class browser (more or less like any class browser available in a Smalltalk implementation). Then went for the "dynamic" side (objects), and implement an inspector, the debugger, profiler and more.
At some point in the project I realized that the main goal should be to keep the API as simple as possible so it would be relatively easy to implement it in any Smalltalk (Bee, Pharo, Dolphin, Squeak, etc.) and have Webside for free.

Here's the some aspects I found intersting along the project:
* Having a simple API would easy the implementation in any Smalltalk falvor to get Webside working on such Smalltalk.
* Having a Smalltalk IDE implemented in JavaScript would allow contributions from the JavaScript community (much greater than the Smalltalk one) to enhance, change and/or extend Webside in any direction.
* Not only this IDE could be used to work on any Smalltalk back-end but also it could allow several developers to work on the same Smalltalk instance (image), making Webside a collaborative environment naturally.

Of course, no one template will serve all projects since your needs may be different. So I'll be adding more in the near future. You may also suggest changes by forking this repo and creating a pull request or opening an issue.

A list of commonly used resources that I find helpful are listed in the acknowledgements.

### Built With
Webside is built using:
* [ReactJS](https://reactjs.org)
* [Material-UI](https://material-ui.com)

<!-- GETTING STARTED -->
## Getting Started

This is an example of how you may give instructions on setting up your project locally.
To get a local copy up and running follow these simple example steps.

### Prerequisites

This is an example of how to list things you need to use the software and how to install them.
* npm
```sh
npm install npm@latest -g
```

### Installation

1. Get a free API Key at [https://example.com](https://example.com)
2. Clone the repo
```sh
git clone https://github.com/your_username_/Project-Name.git
```
3. Install NPM packages
```sh
npm install
```
4. Enter your API in `config.js`
```JS
const API_KEY = 'ENTER YOUR API';
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

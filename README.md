# elm-countdown-problem
* Pick input numbers greater than 0
* Pick a target number greater than 0
* Find all expressions that lead to target using operators `+`, `-`, `x`, `/`
* All sub-expressions should be numbers greater than 0
* Inspired by **Graham Huttons's** paper: [pdf](http://www.cs.nott.ac.uk/~pszgmh/countdown.pdf)

### [Live Demo](https://rajasharan.github.io/elm-countdown-problem)

### Dev setup
```sh
$ elm-reactor
Listening on http://localhost:8000/
```

### Compilation
```sh
$ elm make Main.elm --output build/main.[js|html]
$ cd build
$ lite-server

# Run local webserver using lite-server or python or any framework of choice
# Navigate to .html file where the local server is deployed
```

### [License](https://github.com/rajasharan/elm-countdown-problem/blob/master/LICENSE)
The MIT License (MIT)

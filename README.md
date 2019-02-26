[![Build Status](https://travis-ci.org/avh4/elm-github-v3.svg?branch=master)](https://travis-ci.org/avh4/elm-github-v3)
[![Latest Version](https://img.shields.io/elm-package/v/avh4/elm-github-v3.svg?label=version)](https://package.elm-lang.org/packages/avh4/elm-github-v3/latest/)


# elm-github-v3

This is an unofficial Elm wrapper for the [GitHub REST v3 API](https://developer.github.com/v3/).
The implementation is currently very incomplete
(I've only implemented the exact requests, input parameters, and output decode that I've needed),
but I decided to publish this in case it can save others some work.
Pull requests to make the implementation more complete are welcome.


## Example usage

```sh
elm install avh4/elm-github-v3
```

```elm
import Github

getPullRequestTitles : Cmd (Result String (List String))
getPullRequestTitles =
    Github.getPullRequests
        { authToken = "123..."
        , repo = "avh4/elm-format"
        }
        |> Task.map (List.map .title)
        |> Task.attempt identity
```

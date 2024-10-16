
<!-- README.md is generated from README.Rmd. Please edit that file -->

# elmer <a href="https://hadley.github.io/elmer/"><img src="man/figures/logo.png" align="right" height="138" alt="elmer website" /></a>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/hadley/elmer/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/hadley/elmer/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of elmer is to provide a user friendly wrapper over the most
common llm providers. Major design goals include support for streaming
and making it easy to register and call R functions.

## Installation

You can install the development version of elmer from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("hadley/elmer")
```

## Prerequisites

Depending on which backend you use, you’ll need to set the appropriate
environment variable in your `~/.Renviron` (an easy way to open that
file is to call `usethis::edit_r_environ()`):

- For `chat_claude()`, set `ANTHROPIC_API_KEY` using the key from
  <https://console.anthropic.com/account/keys>.
- For `chat_gemini()`, set `GOOGLE_API_KEY` using the key from
  <https://aistudio.google.com/app/apikey>.
- For `chat_openai()` set `OPENAI_API_KEY` using the key from
  <https://platform.openai.com/account/api-keys>.

## Using elmer

You chat with elmer in several different ways, depending on whether you
are working interactively or programmatically. They all start with
creating a new chat object:

``` r
library(elmer)

chat <- chat_openai(
  model = "gpt-4o-mini",
  system_prompt = "You are a friendly but terse assistant.",
  echo = TRUE
)
```

Chat objects are stateful: they retain the context of the conversation,
so each new query can build on the previous ones. This is true
regardless of which of the various ways of chatting you use.

### Interactive chat console

The most interactive, least programmatic way of using elmer is to chat
with it directly in your R console with `live_console(chat)` or in your
browser with `live_browser()`.

``` r
live_console(chat)
#> ╔════════════════════════════════════════════════════════╗
#> ║  Entering chat console. Use """ for multi-line input.  ║
#> ║  Press Ctrl+C to quit.                                 ║
#> ╚════════════════════════════════════════════════════════╝
#> >>> Who were the original creators of R?
#> R was originally created by Ross Ihaka and Robert Gentleman at the University of
#> Auckland, New Zealand.
#>
#> >>> When was that?
#> R was initially released in 1995. Development began a few years prior to that,
#> in the early 1990s.
```

The chat console is useful for quickly exploring the capabilities of the
model, especially when you’ve customized the chat object with tool
integrations (see below).

Again, keep in mind that the chat object retains state, so when you
enter the chat console, any previous interactions with that chat object
are still part of the conversation, and any interactions you have in the
chat console will persist even after you exit back to the R prompt.

### Interactive method call

The second most interactive way to chat using elmer is to call the
`chat()` method.

``` r
chat$chat("What preceding languages most influenced R?")
#> R was primarily influenced by the S programming language, particularly S-PLUS.
#> Other languages that had an impact include Scheme and various data analysis
#> languages.
```

If you initialize the chat object with `echo = TRUE`, as we did above,
the `chat` method streams the response to the console as it arrives.
When the entire response is received, it is returned as a character
vector (invisibly, so it’s not printed twice).

This mode is useful when you want to see the response as it arrives, but
you don’t want to enter the chat console.

#### Vision (image input)

If you want to ask a question about an image, you can pass one or more
additional input arguments using `content_image_file()` and/or
`content_image_url()`.

``` r
chat$chat(
  content_image_url("https://www.r-project.org/Rlogo.png"),
  "Can you explain this logo?"
)
#> The logo of R features a stylized letter "R" in blue, enclosed in an oval shape that resembles the letter "O,"
#> signifying the programming language's name. The design conveys a modern and professional look, reflecting its use
#> in statistical computing and data analysis. The blue color often represents trust and reliability, which aligns
#> with R's role in data science.
```

The `content_image_url` function takes a URL to an image file and sends
that URL directly to the API. The `content_image_file` function takes a
path to a local image file and encodes it as a base64 string to send to
the API. Note that by default, `content_image_file` automatically
resizes the image to fit within 512x512 pixels; set the `resize`
parameter to `"high"` if higher resolution is needed.

### Programmatic chat

If you don’t want to see the response as it arrives, you can turn off
echoing by leaving off the `echo = TRUE` argument to `chat_openai()`.

``` r
chat <- chat_openai(
  model = "gpt-4o-mini",
  system_prompt = "You are a friendly but terse assistant."
)
chat$chat("Is R a functional programming language?")
#> [1] "Yes, R supports functional programming concepts. It allows functions to be first-class objects, supports higher-order functions, and encourages the use of functions as core components of code. However, it also supports procedural and object-oriented programming styles."
```

This mode is useful for programming using elmer, when the result is
either not intended for human consumption or when you want to process
the response before displaying it.

## Learning more

- Learn more about streaming and async APIs in
  `vignette("streaming-async")`.
- Learn more about tool calling (aka function calling) in
  `vignette("tool-calling")`.

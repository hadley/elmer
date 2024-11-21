
<!-- README.md is generated from README.Rmd. Please edit that file -->

# elmer <a href="https://elmer.tidyverse.org"><img src="man/figures/logo.png" align="right" height="138" alt="elmer website" /></a>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/tidyverse/elmer/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tidyverse/elmer/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of elmer is to provide a user friendly wrapper around the APIs
for large lanuage model (LLM) providers, making it easy to use LLMs from
R. elmer provides a rich set of features including support for streaming
outputs, tool/function calling, structured data output and more.

(Looking for something similar to elmer for python? Check out
[chatlas](https://github.com/cpsievert/chatlas)!)

## Installation

You can install the development version of elmer from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("tidyverse/elmer")
```

## Providers

elmer supports a number of model providers:

- Anthropic’s Claude: `chat_claude()`.
- AWS Bedrock: `chat_bedrock()`.
- Azure OpenAI: `chat_azure()`.
- Databricks: `chat_databricks()`.
- GitHub model marketplace: `chat_github()`.
- Google Gemini: `chat_gemini()`.
- Groq: `chat_groq()`.
- Ollama local models: `chat_ollama()`.
- OpenAI: `chat_openai()`.
- perplexity.ai: `chat_perplexity()`.
- Snowflake Cortex: `chat_cortex()`.

## Model choice

If you’re using elmer inside your organisation, you’ll be limited to
what your org allows, which is likely to be one provided by a big cloud
provider, e.g. `chat_azure()`, `chat_bedrock()`, `chat_databricks()`, or
`chat_snowflake()`. If you’re using elmer for your own personal
exploration, you have a lot more freedom, so we recommend starting with
one of the following:

- I’d recommend starting with either `chat_openai()` or `chat_claude()`.
  `chat_openai()` defaults to **GPT-4o-mini**, which is good and
  relatively cheap. You might want to try `model = "gpt-4o"` for more
  demanding tasks, or `model = "o1-mini"` if you want to force complex
  reasoning. `chat_claude()` is similarly good and well priced. It
  defaults to **Claude 3.5 Sonnet** which we have found to the be the
  best for writing code.

- Try `chat_gemini()` if you want to put a lot of data in the prompt.
  This provider defaults to the **Gemini 1.5 Flash** model which
  supports 1 million tokens, compared to 200k for Claude 3.5 Sonnet and
  128k for GPT 4o mini.

- Use [Ollama](https://ollama.com) with `chat_ollama()` to run models on
  your own computer. The biggest models you can run locally aren’t as
  good as the state of the art hosted models, but they also don’t share
  your data and and are effectively free.

## Using elmer

You can work with elmer in several different ways, depending on whether
you are working interactively or programmatically. They all start with
creating a new chat object:

``` r
library(elmer)

chat <- chat_openai(
  model = "gpt-4o-mini",
  system_prompt = "You are a friendly but terse assistant.",
)
```

Chat objects are stateful: they retain the context of the conversation,
so each new query can build on the previous ones. This is true
regardless of which of the various ways of chatting you use.

### Interactive chat console

The most interactive and least programmatic way of using elmer is to
chat directly in your R console or browser with `live_console(chat)` or
`live_browser()`.

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

Keep in mind that the chat object retains state, so when you enter the
chat console, any previous interactions with that chat object are still
part of the conversation, and any interactions you have in the chat
console will persist even after you exit back to the R prompt.

Interactive chat is useful for quickly exploring the capabilities of the
model, especially when you’ve customized the chat object with tool
integrations (see below).

### Interactive method call

The second most interactive way to chat using elmer is to call the
`chat()` method.

``` r
chat$chat("What preceding languages most influenced R?")
#> R was primarily influenced by the S programming language, particularly S-PLUS.
#> Other languages that had an impact include Scheme and various data analysis
#> languages.
```

If you initialize the chat object in the global environment, the `chat`
method streams the response to the console as it arrives. When the
entire response is received, it is returned as a character vector
(invisibly, so it’s not printed twice).

If you want to ask a question about an image, you can pass one or more
additional input arguments using `content_image_file()` and/or
`content_image_url()`.

``` r
chat$chat(
  content_image_url("https://www.r-project.org/Rlogo.png"),
  "Can you explain this logo?"
)
#> The logo of R features a stylized letter "R" in blue, enclosed in an oval
#> shape that resembles the letter "O," signifying the programming language's
#> name. The design conveys a modern and professional look, reflecting its use
#> in statistical computing and data analysis. The blue color often represents
#> trust and reliability, which aligns with R's role in data science.
```

This mode is useful when you want to see the response as it arrives, but
you don’t want to enter the chat console.

### Programmatic chat

If you create the chat object inside a function, live streaming is
automatically suppressed and `$chat()` returns the result as a string:

``` r
my_function <- function() {
  chat <- chat_openai(
    model = "gpt-4o-mini",
    system_prompt = "You are a friendly but terse assistant.",
  )
  chat$chat("Is R a functional programming language?")
}
my_function()
#> [1] "Yes, R supports functional programming concepts. It allows functions to
#> be first-class objects, supports higher-order functions, and encourages the
#> use of functions as core components of code. However, it also supports
#> procedural and object-oriented programming styles."
```

You can manually control this behaviour with the `echo` argument.

This mode is useful for programming using elmer , when the result is
either not intended for human consumption or when you want to process
the response before displaying it.

## Learning more

- Learn how to design your prompt in `vignette("prompt-design")`.
- Learn more about tool/function calling in `vignette("tool-calling")`.
- Learn more about streaming and async APIs in
  `vignette("streaming-async")`.
- Learn how to extract structured data in `vignette("structured-data")`.

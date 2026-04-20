# Register ggpaintr with an ellmer chat session

One-line registration of the ggpaintr docs tool on an existing
[ellmer](https://ellmer.tidyverse.org/reference/ellmer-package.html)
`Chat` object. The tool wraps
[`ptr_llm_topic()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_llm_topic.md)
so the LLM can pull focused, runnable examples on demand instead of
loading every topic into the system prompt.

## Usage

``` r
ptr_llm_register(chat, tool_name = "ggpaintr_docs")
```

## Arguments

- chat:

  An ellmer `Chat` object (from
  [`ellmer::chat_anthropic()`](https://ellmer.tidyverse.org/reference/chat_anthropic.html),
  [`ellmer::chat_openai()`](https://ellmer.tidyverse.org/reference/chat_openai.html),
  etc.).

- tool_name:

  String. The name the LLM will call the tool under. Defaults to
  `"ggpaintr_docs"`; override if it would collide with another tool you
  already registered.

## Value

The `chat` object (invisibly), for piping.

## Details

The set of valid topic names is snapshotted at registration time using
[`ptr_llm_topics()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_llm_topics.md),
so the LLM cannot request a topic that does not exist. If you upgrade
ggpaintr in the same session and new topics are added, call this again
on a fresh chat.

This function does *not* set the chat's system prompt. Pass
[`ptr_llm_primer()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_llm_primer.md)
to the `system_prompt =` argument of your `chat_*()` constructor so the
model knows when to reach for the tool.

## See also

[`ptr_llm_primer()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_llm_primer.md),
[`ptr_llm_topic()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_llm_topic.md),
[`ptr_llm_topics()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_llm_topics.md)

## Examples

``` r
if (FALSE) { # \dontrun{
library(ellmer)
chat <- chat_anthropic(system_prompt = ptr_llm_primer())
ptr_llm_register(chat)
chat$chat("Build a Shiny app where the user picks X and Y columns from mtcars.")
} # }
```

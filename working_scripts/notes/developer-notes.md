# Developer Notes

## Purpose

This file is for developer-written initial thoughts, rough ideas, and informal planning.

-   It is maintained by developers, not by AI tools.
-   AI tools should not update this file unless the user explicitly asks for that exact file to be edited.
-   This is not part of the canonical session-knowledge system.

## Notes

1.  add instruction or modify the current prompts. Better suggestions?

-   "keep the layer of xxx"
-   "text for xxx"
-   expr for facet_wrap argument 1:

2.  shiny app theme? redesign the current UI

3.  test cases that can break ggpaintr

4.  test cases of super complicated ggplot2 formula

5.  make functions used by `paintr_app_components` external
  - user should be able to write wrapper function like `ggpaintr_app` on their own
  using exported functions of `ggpaintr` package
  
6.  silence the message in console by default.

[x] build the harness? which md files should be actively updated?

# <https://shiny.posit.co/r/articles/share/function/>

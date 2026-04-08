# Developer Notes

## Purpose

This file is for developer-written initial thoughts, rough ideas, and informal planning.

-   It is maintained by developers, not by AI tools.
-   AI tools should not update this file unless the user explicitly asks for that exact file to be edited.
-   This is not part of the canonical session-knowledge system.

## Notes

0.  rename `copy_rules`

1.  development goal as a prompt?

2.  formula driven redesign?

    0.  Add a short roadmap note explaining how to resolve formula-string fragility later without breaking the authoring model:

        -   keep formula strings as the author-facing input;

        -   introduce an internal compiled runtime contract derived once from paintr_obj;

        -   have runtime completion consume that contract instead of re-deriving behavior from raw expression walks and companion-id conventions.

3.  test cases that can break ggpaintr

    -   bug assessment? generate edge cases that can effectively break ggpaintr

4.  shiny app theme? redesign the current UI

5.  test cases of super complicated ggplot2 formula

6.  silence the message in console by default.

# <https://shiny.posit.co/r/articles/share/function/>

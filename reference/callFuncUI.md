# Calls the selected ui functions by the name of the key

Calls the selected ui functions by the name of the key

## Usage

``` r
callFuncUI(name, defaultArgs, scope, extraFunc = NULL, extraFuncArgs = NULL)
```

## Arguments

- name:

  name of the key

- defaultArgs:

  some default arguments passed to the ui functions

- scope:

  `scope` can be one of `mapping`, `geom_args`, or `plot_settings`

- extraFunc:

  optional. A named list of extra functions provided by the user. For
  example `list(param1 = my_func1, param2 = my_func2)`

- extraFuncArgs:

  optional. A list of function arguments provided by the user. Function
  arguments of one function should be formed in a list as one element of
  `extraFuncArgs` For example
  `list(param1 = list(my_func1_arg1, my_func1_arg2), param2 = list(my_func2_arg1, my_func2_arg2))`

## Value

function call of the selected function

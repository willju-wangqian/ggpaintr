An expert would read this test suite in layers, from “test harness” to “pipeline stage” to “coverage gaps.”

**Reading Order**

1. Start with [tests/testthat.R](/Users/willju/Research/ggpaintr/tests/testthat.R).  
This file tells you almost nothing about behavior; it only says the package uses `testthat`.

2. Then read [helper-fixtures.R](/Users/willju/Research/ggpaintr/tests/testthat/helper-fixtures.R).  
This is the real entrypoint for understanding the suite. It:
- loads the current `paintr2` source directly with `source(...)`
- defines upload mock data
- defines reusable supported and unsupported use cases

If you understand this file, the rest of the tests become much easier to scan.

3. Then read tests in pipeline order:
- [test-parse-formula.R](/Users/willju/Research/ggpaintr/tests/testthat/test-parse-formula.R)
- [test-placeholders.R](/Users/willju/Research/ggpaintr/tests/testthat/test-placeholders.R)
- [test-upload.R](/Users/willju/Research/ggpaintr/tests/testthat/test-upload.R)
- [test-complete-expr.R](/Users/willju/Research/ggpaintr/tests/testthat/test-complete-expr.R)
- [test-plot-build.R](/Users/willju/Research/ggpaintr/tests/testthat/test-plot-build.R)
- [test-export-shiny.R](/Users/willju/Research/ggpaintr/tests/testthat/test-export-shiny.R)
- [test-supported-use-cases.R](/Users/willju/Research/ggpaintr/tests/testthat/test-supported-use-cases.R)
- [test-unsupported-use-cases.R](/Users/willju/Research/ggpaintr/tests/testthat/test-unsupported-use-cases.R)

4. Finish with the human QA file:
- [manual-checklist-paintr2.md](/Users/willju/Research/ggpaintr/tests/manual/manual-checklist-paintr2.md)

That tells you what the automated tests deliberately do not cover.

**What Each Test File Does**

[helper-fixtures.R](/Users/willju/Research/ggpaintr/tests/testthat/helper-fixtures.R)  
Think of this as “test infrastructure + canonical examples.”
- `fixture_path()` resolves fixture files
- `mock_upload_input()` simulates Shiny file upload objects
- `supported_use_cases` defines formulas that should parse, complete, and build
- `unsupported_use_cases` defines formulas that should fail at specific stages

[test-parse-formula.R](/Users/willju/Research/ggpaintr/tests/testthat/test-parse-formula.R)  
This file tests the parser contract.
- Does `paintr_formula()` return a `paintr_obj`?
- Does it split top-level `+` layers correctly?
- Does it detect placeholders?
- Does it avoid treating quoted `"var"` as a placeholder?

This is the “AST and metadata” layer.

[test-placeholders.R](/Users/willju/Research/ggpaintr/tests/testthat/test-placeholders.R)  
This tests placeholder substitution semantics.
- `var` gets inserted into mappings
- `text` becomes quoted text
- `num` becomes numeric code
- `expr` gets parsed into the final expression

This is the “placeholder replacement” layer.

[test-upload.R](/Users/willju/Research/ggpaintr/tests/testthat/test-upload.R)  
This isolates upload behavior.
- CSV and RDS are read correctly
- default and custom dataset names are resolved correctly
- unsupported file types error correctly
- `output_embed_var()` waits until upload exists, then populates `var` controls

This is the “upload contract” layer.

[test-complete-expr.R](/Users/willju/Research/ggpaintr/tests/testthat/test-complete-expr.R)  
This tests the final expression assembly step.
- unchecked layers are removed
- upload-backed object names enter the final code
- uploaded objects are placed into `eval_env`
- malformed `expr` input errors

This is the “final expression and eval env” layer.

[test-plot-build.R](/Users/willju/Research/ggpaintr/tests/testthat/test-plot-build.R)  
This tests that completed expressions actually build plots.
- static-data formula builds a `ggplot`
- upload-backed formula builds a `ggplot`

This is the “execution” layer.

[test-export-shiny.R](/Users/willju/Research/ggpaintr/tests/testthat/test-export-shiny.R)  
This tests exported app generation at the file/text level.
- file gets written
- output parses as R
- generated app contains expected runtime hooks
- upload formulas preserve upload-aware code paths

This is the “export artifact” layer.

[test-supported-use-cases.R](/Users/willju/Research/ggpaintr/tests/testthat/test-supported-use-cases.R)  
This is a compact integration test over the canonical “should work” formulas from `helper-fixtures.R`.

[test-unsupported-use-cases.R](/Users/willju/Research/ggpaintr/tests/testthat/test-unsupported-use-cases.R)  
This is the guardrail file.
- multiple expressions should fail
- `var` without data should fail
- unknown data objects should fail
- quoted placeholders should not behave like real placeholders

This is important because it defines the intended boundary of support.

[manual-checklist-paintr2.md](/Users/willju/Research/ggpaintr/tests/manual/manual-checklist-paintr2.md)  
This is the human interaction layer:
- real app rendering
- upload interaction
- layer toggling
- exported-app smoke testing

**How An Expert Mentally Maps These Tests**

They usually map each file to one stage of the runtime:

1. Parse formula
2. Detect placeholders
3. Resolve input values
4. Build completed expression
5. Build plot
6. Export app
7. Define supported vs unsupported boundary
8. Manually verify interaction behavior

That mental model makes failures much easier to diagnose.

For example:
- if `test-parse-formula.R` fails, the parser/metadata changed
- if `test-upload.R` fails, the upload contract changed
- if `test-complete-expr.R` fails, substitution or eval env changed
- if `test-plot-build.R` fails, execution semantics changed
- if `test-export-shiny.R` fails, generated app text/runtime changed

**What The Tests Do Not Fully Cover Yet**

An expert would also notice the current gaps:
- no browser-level automated Shiny interaction tests
- no direct assertions yet for spaced column names fixture behavior
- no explicit tests yet for `reorder()`-style formulas
- no direct tests for improved UI error presentation
- export tests are syntax/text-based, not full runtime execution of the exported app

So the suite is strong on core logic, but still light on interactive UI behavior.

**Best Way To Study It Quickly**

If you want to truly understand it fast, do this:
1. Read [helper-fixtures.R](/Users/willju/Research/ggpaintr/tests/testthat/helper-fixtures.R)
2. Read [test-parse-formula.R](/Users/willju/Research/ggpaintr/tests/testthat/test-parse-formula.R)
3. Read [test-complete-expr.R](/Users/willju/Research/ggpaintr/tests/testthat/test-complete-expr.R)
4. Read [test-upload.R](/Users/willju/Research/ggpaintr/tests/testthat/test-upload.R)
5. Then skim the rest

That order gives you the architecture first, then the critical transformation logic, then the upload-specific path.

If you want, next I can do one of two useful follow-ups:
1. annotate each test file line by line in plain English
2. produce a “test map” showing which function each test covers and what is still untested

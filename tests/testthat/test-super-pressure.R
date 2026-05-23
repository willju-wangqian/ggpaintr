# test-super-pressure.R — single-file home for the five super-app pressure tests
# (ADR 0013). Each app lives between an anchor-delimited region. The anchors
# exist so the five app plans (PLAN-02 .. PLAN-06) edit non-overlapping
# regions of this file in independent worktrees.
#
# Anchor pairs (locked by ADR 0013 D3 + PLAN-01; later plans MUST NOT remove
# or rename them, only fill them):
#   # >>> super-1  begin / # <<< super-1  end   -> filled by PLAN-02
#       (super-app-1  kitchen-sink)
#   # >>> super-2a begin / # <<< super-2a end   -> filled by PLAN-03
#       (super-app-2a upload registry)
#   # >>> super-2b begin / # <<< super-2b end   -> filled by PLAN-04
#       (super-app-2b customsource splice)
#   # >>> super-3  begin / # <<< super-3  end   -> filled by PLAN-05
#       (super-app-3  L3 multi-shared plotly)
#   # >>> super-4  begin / # <<< super-4  end   -> filled by PLAN-06
#       (super-app-4  user-css safety-adversarial)
#
# Each region holds at most ONE `test_that()` block. The sentinel-propagation
# helpers used inside live in `helper-super-pressure.R` (auto-loaded by
# testthat from this directory). Per ADR 0013, every assertion in this file
# names an exact sentinel + an exact position regex + an explicit mode — no
# presence-style proxies (see project memory `e2e-assertion-weakness-lens`).

# >>> super-1 begin
# (PLAN-02 inserts the super-app-1 kitchen-sink test_that() block here.)
# <<< super-1 end

# >>> super-2a begin
# (PLAN-03 inserts the super-app-2a upload-registry test_that() block here.)
# <<< super-2a end

# >>> super-2b begin
# (PLAN-04 inserts the super-app-2b customsource-splice test_that() block here.)
# <<< super-2b end

# >>> super-3 begin
# (PLAN-05 inserts the super-app-3 L3-multi-shared-plotly test_that() block here.)
# <<< super-3 end

# >>> super-4 begin
# (PLAN-06 inserts the super-app-4 user-css safety-adversarial test_that() block here.)
# <<< super-4 end

{incl_app, katja_vmstats, details}.
{excl_mods, katja_vmstats, [
  katja_vmstats_metrics_test,
  eunit_SUITE,
  riemann_SUITE
]}.
{export, "logs/all.coverdata"}.

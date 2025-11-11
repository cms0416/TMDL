# %% ---------------------------------------------------------------------------
# wq_analysis 패키지 초기화

from .config import BASE_DIR, WQ_DIR, OUTPUT_DIR
from .season import add_season
from .weather import (
    load_weather_data,
    build_weather_indicators,
)
from .tmdl import (
    AnalysisConfig,
    load_target_quality,
    load_total_network,
    build_achievement_tables,
    load_local_wq,
)
from .stats_tools import compute_correlations
from .plot_tools import (
    plot_corr_heatmap,
    plot_flow_quality,
    plot_rain_quality,
    plot_year_box_with_flow,
    plot_rain3_tp_scatter,
    plot_event_boxplot,
)

__all__ = [
    "BASE_DIR", "WQ_DIR", "OUTPUT_DIR",
    "add_season",
    "load_weather_data", "build_weather_indicators",
    "AnalysisConfig",
    "load_target_quality", "load_total_network",
    "build_achievement_tables", "load_local_wq",
    "compute_correlations",
    "plot_corr_heatmap", "plot_flow_quality", "plot_rain_quality",
    "plot_year_box_with_flow", "plot_rain3_tp_scatter", "plot_event_boxplot",
]

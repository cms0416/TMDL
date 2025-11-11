# %% ---------------------------------------------------------------------------
from pathlib import Path
import pandas as pd

from wq_analysis import (
    AnalysisConfig,
    load_weather_data,
    build_weather_indicators,
    load_target_quality,
    load_total_network,
    build_achievement_tables,
    load_local_wq,
    compute_correlations,
    plot_corr_heatmap,
)

# %% ---------------------------------------------------------------------------
cfg = AnalysisConfig(
    basin="주천A",
    met_station="안흥",
    pollutant="TP",
    target_col="TP_목표수질",
    start_year=2015,
    end_year=2025,
    local_wq_file=Path(
        "E:/Coding/TMDL/수질분석/수질측정망/주천A_수질측정망_2015_2024.xlsx"
    ),
    pollutant_label="T-P",
    outlier_filter=None,  # 필요하면 {"col": "TP", "max": 2.0}
)

# %% ---------------------------------------------------------------------------
# 1. 기상자료
daily, hourly = load_weather_data(cfg.met_station)
wx = build_weather_indicators(daily, hourly)

# 2. 총량측정망
target = load_target_quality(cfg)
total = load_total_network(cfg, target, wx["기상지표_일별"])

# 3. 달성률 테이블
ach = build_achievement_tables(
    cfg, total, wx["강수량_월별_평균"]
)

# 4. 상관분석
corr_cols = [
    cfg.pollutant, "SS", "유량",
    "누적강수_3일", "누적강수_4일", "누적강수_5일",
    "평균기온", "일강수량", "COD", "수온",
    "TOC", "TN", "BOD", "pH", "EC", "DO",
]
corr_matrix, pval_matrix = compute_correlations(total, corr_cols)

# 5. 히트맵 예시
# tp_corr = corr_matrix[cfg.pollutant].drop(cfg.pollutant)
# x_order = [cfg.pollutant] + tp_corr.sort_values(ascending=False).index.tolist()
# y_order = tp_corr.sort_values(ascending=True).index.tolist() + [cfg.pollutant]
# corr_sorted = corr_matrix.loc[y_order, x_order]
# plot_corr_heatmap(
#     corr_sorted,
#     pval_matrix,
#     save_path=f"E:/Coding/TMDL/수질분석/Output/Plot/{cfg.basin}_상관계수.png",
# )

# 6. 결과 저장 (필요 시)
out_path = Path(
    f"E:/Coding/TMDL/수질분석/Output/수질분석_{cfg.basin}_파이썬.xlsx"
)
with pd.ExcelWriter(out_path) as writer:
    total.to_excel(writer, sheet_name="수질현황", index=False)
    ach["달성률_유황구간별"].to_excel(writer, sheet_name="달성률_유황구간별", index=False)
    ach["달성률_계절별"].to_excel(writer, sheet_name="달성률_계절별", index=False)
    ach["달성률_월별"].to_excel(writer, sheet_name="달성률_월별", index=False)
    ach["달성률_연도별"].to_excel(writer, sheet_name="달성률_연도별", index=False)
    corr_matrix.to_excel(writer, sheet_name="상관계수", index=True)
    pval_matrix.to_excel(writer, sheet_name="p값", index=True)

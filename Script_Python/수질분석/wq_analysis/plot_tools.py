# %% ---------------------------------------------------------------------------
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
import matplotlib.patches as mpatches
from matplotlib.ticker import FuncFormatter, ScalarFormatter
import seaborn as sns

# 공통 계절 색상
SEASON_PALETTE = {
    "봄": "#F8766D",
    "여름": "#00BA38",
    "가을": "#619CFF",
    "겨울": "#C77CFF",
}

sns.set_theme(style="whitegrid", font="NanumGothic", font_scale=1.0)

# %% ---------------------------------------------------------------------------
def plot_corr_heatmap(corr_sorted: pd.DataFrame,
                      pval_matrix: pd.DataFrame,
                      save_path: str | None = None):
    annot = corr_sorted.copy()
    pval_sorted = pval_matrix.loc[annot.index, annot.columns]
    annot = annot.round(2).astype(str)
    annot[pval_sorted > 0.05] = ""

    plt.figure(figsize=(11, 10))
    ax = sns.heatmap(
        corr_sorted,
        annot=annot, fmt="",
        cmap="coolwarm", center=0,
        linewidths=0.5,
        cbar_kws={"shrink": 0.8},
        annot_kws={"weight": "bold", "fontsize": 12},
    )
    cbar = ax.collections[0].colorbar
    cbar.ax.set_xlabel("상관계수", fontsize=14, weight="bold", labelpad=10)
    cbar.ax.xaxis.set_label_position("top")
    plt.xticks(fontsize=13, weight="bold", rotation=45, ha="right")
    plt.yticks(fontsize=13, weight="bold")
    plt.tight_layout(pad=1.5)
    plt.subplots_adjust(right=1.05)

    if save_path:
        plt.savefig(save_path, dpi=300)
    plt.show()

# 이후 함수들은 구조만 제시. 실제 수치는 필요에 맞게 조정.

def plot_flow_quality(df: pd.DataFrame,
                      pollutant: str,
                      target_value: float,
                      basin: str,
                      y_log: bool = True,
                      save_path: str | None = None):
    """유량백분율-유량 / 농도 이중축 그래프."""
    data = df.copy()
    season_colors = data["계절"].map(SEASON_PALETTE)

    fig, ax1 = plt.subplots(figsize=(8.4, 5))
    ax2 = ax1.twinx()

    # 유량 막대
    ax2.bar(
        data["유량백분율"], data["유량"],
        width=0.3, color="steelblue",
        edgecolor="steelblue", alpha=1.0, label="유량",
    )
    ax2.yaxis.set_major_formatter(
        FuncFormatter(lambda x, _: f"{int(x):,}")
    )
    ax2.grid(False)

    # 수질 산점
    sc = ax1.scatter(
        data["유량백분율"], data[pollutant],
        c=season_colors, edgecolor="black",
        s=40, alpha=0.6, label=f"{pollutant}",
    )

    if y_log:
        ax1.set_yscale("log")
        ax1.yaxis.set_major_formatter(ScalarFormatter())
        ax1.ticklabel_format(axis="y", style="plain")

    ax1.set_xticks(range(0, 101, 10))
    ax1.set_xticklabels([f"{x}%" for x in range(0, 101, 10)])
    ax1.grid(which="both", linestyle="-", linewidth=0.5, alpha=0.7)

    # 목표수질
    ax1.axhline(
        y=target_value,
        color="red", linestyle="dashed",
        linewidth=1.2, label="목표수질",
    )

    # 유황구간 기준선
    for x in [0, 10, 40, 60, 90, 100]:
        ax1.axvline(x=x, linestyle="dashed", linewidth=1, color="gray")

    ax1.set_xlabel("유량 백분율(%)", fontsize=14, weight="bold")
    ax1.set_ylabel(f"{pollutant} (mg/L)", fontsize=14, weight="bold")
    ax2.set_ylabel("유량(㎥/s)", fontsize=14, weight="bold")

    # 범례 구성은 필요 시 기존 패턴대로 추가
    plt.tight_layout()
    if save_path:
        plt.savefig(save_path, dpi=300)
    plt.show()

# 나머지 plot 함수(강수-수질, 연도별 박스플롯 등)는 동일 패턴으로 재사용 가능하므로 생략.

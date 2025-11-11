# %% ---------------------------------------------------------------------------
from dataclasses import dataclass
from pathlib import Path
import pandas as pd
import numpy as np

from .config import WQ_DIR, OUTPUT_DIR
from .season import add_season

# %% ---------------------------------------------------------------------------
@dataclass
class AnalysisConfig:
    basin: str          # 단위유역
    met_station: str    # 기상대
    pollutant: str      # "TP" or "BOD"
    target_col: str     # "TP_목표수질" or "BOD_목표수질"
    start_year: int
    end_year: int
    local_wq_file: Path | None = None   # 유역내 수질측정망 파일 (옵션)
    pollutant_label: str = ""           # 그래프용 라벨 (예: "T-P", "BOD")
    outlier_filter: dict | None = None  # 예: {"col": "TP", "max": 2.0}

# %% ---------------------------------------------------------------------------
def load_target_quality(cfg: AnalysisConfig) -> float:
    path = WQ_DIR / "목표수질.xlsx"
    df = pd.read_excel(path)
    val = df.loc[df["총량지점명"] == cfg.basin, cfg.target_col].values[0]
    return float(val)

# %% ---------------------------------------------------------------------------
def load_total_network(cfg: AnalysisConfig,
                       target_value: float,
                       weather_daily: pd.DataFrame) -> pd.DataFrame:
    """총량측정망 자료 로드 및 공통 가공."""
    path = WQ_DIR / "총량측정망_전체_2007_2025.xlsx"
    pol = cfg.pollutant

    df = (
        pd.read_excel(path)
        .dropna(subset=[pol])
        .query(
            "총량지점명.str.contains(@cfg.basin) and "
            "연도 >= @cfg.start_year and 연도 <= @cfg.end_year"
        )
    )

    df = add_season(df, month_column="월")

    # 달성여부
    df["달성여부"] = np.where(df[pol] > target_value, "초과", "달성")

    # 부하량 계산
    load_col = f"측정부하량_{pol}"
    tgt_load_col = f"목표부하량_{pol}"
    df[load_col] = df["유량"] * df[pol] * 86.4
    df[tgt_load_col] = df["유량"] * target_value * 86.4

    # 유량 기반 정렬 및 유황구간
    df = df.sort_values("유량", ascending=False).reset_index(drop=True)
    df["유량크기순서"] = df["유량"].rank(method="min", ascending=False)
    n_valid = df["유량"].notna().sum()
    df["유량백분율"] = df["유량크기순서"] / n_valid * 100

    bins = [0, 10, 40, 60, 90, 100]
    labels = ["홍수기", "풍수기", "평수기", "저수기", "갈수기"]
    df["유황구간"] = pd.cut(
        df["유량백분율"],
        bins=bins,
        labels=labels,
        include_lowest=True,
    )

    # 기상지표(일별) 병합
    df = df.merge(
        weather_daily[
            [
                "일자", "연도", "월",
                "일강수량", "최대시간강우량",
                "고강도강우_발생여부",
                "매우고강도강우_발생여부",
                "강우일", "평균기온",
                "고강도강우_발생여부_3일누적",
                "누적강수_3일", "누적강수_4일", "누적강수_5일",
            ]
        ],
        on=["일자", "연도", "월"],
        how="left",
    )

    # 유황구간 순서형
    df["유황구간"] = pd.Categorical(
        df["유황구간"],
        categories=["갈수기", "저수기", "평수기", "풍수기", "홍수기"],
        ordered=True,
    )

    # 유량크기순서 맨 앞으로 이동
    cols = ["유량크기순서"] + [c for c in df.columns if c != "유량크기순서"]
    df = df[cols]

    return df

# %% ---------------------------------------------------------------------------
def build_achievement_tables(cfg: AnalysisConfig,
                             total_df: pd.DataFrame,
                             monthly_rain: pd.DataFrame):
    """유황구간/계절/월/연도별 달성률 테이블 생성."""
    # 유황구간별
    g = (
        total_df.groupby(["유황구간", "달성여부"])
        .size().unstack(fill_value=0).reset_index()
    )
    g["총계"] = g["달성"] + g["초과"]
    g["달성률"] = (g["달성"] / g["총계"] * 100).round(1)
    # '총계' 열 위치 조정
    total_col = g.pop("총계")
    g.insert(g.columns.get_loc("달성"), "총계", total_col)
    ach_by_flow = g.sort_values("유황구간").reset_index(drop=True)

    # 계절별
    g = (
        total_df.groupby(["계절", "달성여부"])
        .size().unstack(fill_value=0).reset_index()
    )
    g["총계"] = g["달성"] + g["초과"]
    g["달성률"] = (g["달성"] / g["총계"] * 100).round(1)
    total_col = g.pop("총계")
    g.insert(g.columns.get_loc("달성"), "총계", total_col)
    ach_by_season = g

    # 월별
    g = (
        total_df.groupby(["월", "달성여부"])
        .size().unstack(fill_value=0).reset_index()
    )
    g["총계"] = g["달성"] + g["초과"]
    g["달성률"] = (g["달성"] / g["총계"] * 100).round(1)
    g = g.merge(monthly_rain, on="월", how="left")
    total_col = g.pop("총계")
    g.insert(g.columns.get_loc("달성"), "총계", total_col)
    ach_by_month = g

    # 연도별
    pol = cfg.pollutant
    g = (
        total_df.groupby("연도")
        .agg(
            평균농도=(pol, lambda x: round(x.mean(), 3)),
            유량_합계=("유량", "sum"),
            달성=("달성여부", lambda x: (x == "달성").sum()),
        )
        .reset_index()
    )
    초과 = (
        total_df.groupby("연도")["달성여부"]
        .apply(lambda x: (x == "초과").sum())
        .values
    )
    g["초과"] = 초과
    g["총계"] = g["달성"] + g["초과"]
    g["달성률"] = (g["달성"] / g["총계"] * 100).round(1)
    ach_by_year = g[["연도", "초과", "달성", "총계", "달성률", "평균농도", "유량_합계"]]

    return {
        "달성률_유황구간별": ach_by_flow,
        "달성률_계절별": ach_by_season,
        "달성률_월별": ach_by_month,
        "달성률_연도별": ach_by_year,
    }

# %% ---------------------------------------------------------------------------
def load_local_wq(cfg: AnalysisConfig, target_value: float) -> pd.DataFrame:
    """유역 내 수질측정망 자료 로드 및 달성률/계절 정보 부여."""
    if cfg.local_wq_file is None:
        return pd.DataFrame()

    df = pd.read_excel(cfg.local_wq_file)

    # 분석 항목 결측 제거
    df = df.dropna(subset=[cfg.pollutant])

    # 이상치 필터 (옵션)
    if cfg.outlier_filter:
        col = cfg.outlier_filter.get("col", cfg.pollutant)
        max_val = cfg.outlier_filter.get("max", None)
        if max_val is not None:
            df = df[df[col] < max_val]

    df["일자"] = pd.to_datetime(df["일자"])
    df["연도"] = df["일자"].dt.year
    df["월"] = df["일자"].dt.month

    df = add_season(df, month_column="월")

    df["달성여부"] = np.where(df[cfg.pollutant] > target_value, "초과", "달성")

    return df

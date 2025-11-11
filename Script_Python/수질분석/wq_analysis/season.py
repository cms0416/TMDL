# %% ---------------------------------------------------------------------------
import pandas as pd
import numpy as np

def add_season(df: pd.DataFrame, month_column: str = "월") -> pd.DataFrame:
    """월을 기준으로 계절 범주형 변수 추가."""
    df = df.copy()
    conds = [
        df[month_column].between(3, 5),
        df[month_column].between(6, 8),
        df[month_column].between(9, 11),
    ]
    seasons = ["봄", "여름", "가을"]
    df["계절"] = np.select(conds, seasons, default="겨울")
    df["계절"] = pd.Categorical(
        df["계절"], categories=["봄", "여름", "가을", "겨울"], ordered=True
    )
    return df

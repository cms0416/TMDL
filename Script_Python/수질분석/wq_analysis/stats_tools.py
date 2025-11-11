# %% ---------------------------------------------------------------------------
import pandas as pd
import numpy as np
from scipy.stats import pearsonr, mannwhitneyu

# %% ---------------------------------------------------------------------------
def compute_correlations(df: pd.DataFrame, cols: list[str]):
    """상관계수 및 p-value 행렬 계산."""
    df_corr = df[cols].dropna()
    n = len(cols)
    corr_matrix = pd.DataFrame(index=cols, columns=cols, dtype=float)
    pval_matrix = pd.DataFrame(index=cols, columns=cols, dtype=float)

    for c1 in cols:
        for c2 in cols:
            if c1 == c2:
                corr_matrix.loc[c1, c2] = 1.0
                pval_matrix.loc[c1, c2] = 0.0
            else:
                r, p = pearsonr(df_corr[c1], df_corr[c2])
                corr_matrix.loc[c1, c2] = r
                pval_matrix.loc[c1, c2] = p

    return corr_matrix, pval_matrix

# Mann-Whitney 등 추가 검정은 필요 시 함수로 분리해서 사용

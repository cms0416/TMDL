# %% ---------------------------------------------------------------------------
from pathlib import Path
import matplotlib.pyplot as plt

# 기본 경로 설정
BASE_DIR = Path("E:/Coding/TMDL")
WQ_DIR = BASE_DIR / "수질분석"
OUTPUT_DIR = WQ_DIR / "Output"

# 폰트 및 공통 matplotlib 설정
import matplotlib.font_manager as fm  # noqa

plt.rcParams["font.family"] = "NanumGothic"
plt.rcParams["axes.unicode_minus"] = False

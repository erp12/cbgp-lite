
# %%
import os
import glob
import numpy as np
import pandas as pd
import plotly.express as px
import plotly.graph_objects as go
import plotly.figure_factory as ff
from plotly.subplots import make_subplots

# %%
def read_dataset(root: str) -> pd.DataFrame:
    df_list = []
    all_files = glob.glob(f"{root}/**/*.jsonl", recursive=True)
    for file_path in all_files:
        print(f"Reading {file_path}")
        df = pd.read_json(file_path, lines=True)
        parts = file_path.split(os.sep)
        for part in parts:
            if '=' in part:
                key, value = part.split('=')
                df[key] = value
        df_list.append(df)
    df = pd.concat(df_list, ignore_index=True)
    return df

# %%
lineages = read_dataset("data/lineage")
lineages = lineages.sort_values(["problem", "lineage", "step"])
# %%
lineages

# %%
df = lineages.copy()
# %%

# mark first time each code appears within each problem/lineage
is_new_code = ~df.duplicated(subset=["problem", "lineage", "code_id"])

# cumulative number of distinct codes seen so far within each problem/trial
df["cardinality"] = (
    is_new_code
    .groupby([df["problem"], df["lineage"]])
    .cumsum()
)

fig = px.line(
    df[["problem", "lineage", "step", "cardinality"]],
    x="step",
    y="cardinality",
    color="lineage",
    facet_row="problem",
    labels={
        "step": "Number of Mutations in Lineage",
        "cardinality": "Unique Programs",
        "lineage": "Lineage",
    },
    height=1000,
)

problem_order = ["compare-string-lengths", "count-odds", "fuel-cost", "number-io", "vectors-summed"]
y_maxes = {
    "compare-string-lengths": 450,
    "count-odds": 200,
    "fuel-cost": 100,
    "number-io": 150,
    "vectors-summed": 150,
}
for i, problem in enumerate(problem_order, start=1):
    fig.add_shape(
        type="line",
        x0=0,
        y0=0,
        x1=y_maxes[problem],
        y1=y_maxes[problem],
        line=dict(color="grey", dash="dash"),
        row=6-i,
        col=1,
    )
fig.update_yaxes(matches=None)
fig.write_image("plots/pdf/lineage.pdf")
fig.show()

# %%

(
    df
    .query("step == 300")
    .groupby(["problem"])
    .agg(
        avg_cardinality=("cardinality", "mean")
    )
    .assign(inverse_slope=lambda d: 1 / (d["avg_cardinality"] / 300))
)
# %%

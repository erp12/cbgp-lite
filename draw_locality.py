
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
df_list = []
all_files = glob.glob("data/locality/**/*.jsonl", recursive=True)
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

df["redundancy"] = np.select(
    [
        df["phenotype_distance"] == 0,
        df["behavior_distance"] == 0,
    ],
    ["Same Phenotype", "Same Behavior"],
    default="Not redundant"
)

# %%
df
df["problem"].unique()

# %%
# Correlation of genotype distance to phenotype distance
fig = px.density_heatmap(
    df,
    x="genotype_distance",
    y="phenotype_distance",
    facet_row="problem",
    facet_col="umad",
    labels={
        "genotype_distance": "Genome Levenshtein Distance",
        "phenotype_distance": "Program TED",
        "umad": "UMAD Rate",
    },
    height=1000,
)
fig.update_traces(coloraxis=None, showscale=False)
fig.write_image("plots/pdf/ged-by-pred-heatmap.pdf")
fig.show()

# %%
# Correlation of genotype distance to phenotype distance
fig = px.density_heatmap(
    df,
    x="genotype_distance",
    y="phenotype_distance",
    facet_row="problem",
    facet_col="umad",
    labels={
        "genotype_distance": "Genome Levenshtein Distance",
        "phenotype_distance": "Program TED",
        "umad": "UMAD Rate",
    },
    height=1000,
)
fig.update_traces(coloraxis=None, showscale=False)
fig.update_yaxes(range=[0, 20])
fig.write_image("plots/pdf/ged-by-pred-heatmap-clip.pdf")
fig.show()

# %%
# Correlation of genotype distance to phenotype distance
# fig = px.density_contour(
#     df,
#     x="genotype_distance",
#     y="phenotype_distance",
#     facet_row="problem",
#     facet_col="umad",
#     labels={
#         "genotype_distance": "Genome Levenshtein Distance",
#         "phenotype_distance": "Program TED",
#         "umad": "UMAD Rate",
#     },
#     height=1000,
# )
# fig.show()

# %%
# Distribution of program edit distances by mutation strength across time.
fig = px.box(
    df,
    x="phenotype_distance", 
    color="generation",
    facet_row="problem",
    facet_col="umad",
    labels={
        "genotype_distance": "Genome Levenshtein Distance",
        "phenotype_distance": "Program Tree Edit Distance",
        "umad": "UMAD Rate",
        "generation": "Gen",
    },
    height=1000,
    color_discrete_sequence=px.colors.sequential.Viridis,
)
fig.write_image("plots/pdf/ted-by-generation.pdf")
fig.show()

# %%
# (Non-)Synonymous Redundancy
fig = px.histogram(
    df,
    x="genotype_distance", 
    facet_row="problem",
    facet_col="umad",
    color="redundancy",
    labels={
        "genotype_distance": "Genome Levenshtein Distance",
        "phenotype_distance": "Program Tree Edit Distance",
        "redundancy": "Redundancy",
    },
    category_orders={"redundancy": ["Same Phenotype", "Same Behavior", "Not redundant"]},
    color_discrete_sequence=["red", "orange", "blue"],
    height=1000,
    facet_col_spacing=0.06,
)
fig.update_xaxes(matches=None, showticklabels=True)
fig.update_yaxes(matches=None, showticklabels=True)
for r, problem in enumerate(df["problem"].unique()):
    for c, umad in enumerate(df["umad"].unique()):
        df_ = df[(df["problem"] == problem) & (df["umad"] == umad)]
        overall_avg = df_["genotype_distance"].median()
        redundant_avg = df_[df_["phenotype_distance"] == 0]["genotype_distance"].median()
        fig.add_vline(
            x=overall_avg,
            row=r + 1, col=c + 1,
        )
        fig.add_vline(
            x=redundant_avg,
            row=r + 1, col=c + 1,
            line_dash="dash",
        )

fig.update_layout(legend=dict(
    orientation="h",
    yanchor="bottom",
    xanchor="left",
    y=1.05,
))
fig.write_image("plots/pdf/redundant.pdf")
fig.show()
# %%
(
    df
    .groupby(["problem", "umad"])
    .agg(
        median_phenotype_distance=pd.NamedAgg(column="phenotype_distance", aggfunc="median"),
    )
    .reset_index()
    .groupby(["median_phenotype_distance"])
    .agg(
        n=("median_phenotype_distance", "count"),
    )
)
# %%
(
    df
    .groupby(["problem", "umad"])
    .agg(
        n=("problem", "count"),
    )
    .shape
)
# %%

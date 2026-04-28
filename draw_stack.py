
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
stack_depths = read_dataset("data/stack-depth")
# %%
stack_depth_intervals = (
    stack_depths
    .groupby(["problem", "step"])
    .agg(
        stack_depth_mean=("stack_depth", "mean"),
        stack_depth_min=("stack_depth", "min"),
        stack_depth_max=("stack_depth", "max"),
        stack_depth_median=("stack_depth", "median"),
        stack_depth_q1=("stack_depth", lambda x: np.percentile(x, 25)), 
        stack_depth_q3=("stack_depth", lambda x: np.percentile(x, 75))
    )
    .reset_index()
)
# %%
stack_depth_intervals

# %%
# Stack Depth
fig = px.line(
    stack_depth_intervals,
    x="step",
    y="stack_depth_mean",
    facet_row="problem",
    labels={
        "stack_depth_mean": "Stack Depth",
        "step": "Compilation Step",
    },
    height=1000,
)

fig_upper = px.line(
    stack_depth_intervals,
    x='step', 
    y='stack_depth_min',
    color_discrete_sequence=["lightblue"],
    facet_row="problem",
)
fig.add_traces(fig_upper.data)
fig_lower = px.line(
    stack_depth_intervals,
    x='step', 
    y='stack_depth_max',
    color_discrete_sequence=["lightblue"],
    facet_row="problem",
)
fig_lower.update_traces(fill='tonexty')
fig.add_traces(fig_lower.data)

fig.add_traces(go.Scatter(
    x=[None],
    y=[None], 
    mode='markers',
    marker=dict(size=10, color='blue'),
    legendgroup='group1',
    showlegend=True, 
    name='Mean',
))
fig.add_trace(go.Scatter(
    x=[None], y=[None], mode='markers',
    marker=dict(size=10, color='lightblue'),
    legendgroup='group2', showlegend=True, name='Min/Max Range'
))
fig.update_layout(legend=dict(
    orientation="h",
    yanchor="bottom",
    xanchor="left",
    y=1,
    x=0.25,
))
fig.write_image("plots/pdf/stack-depth.pdf")
fig.show()

# %%
ast_sizes = read_dataset("data/stack-ast-sizes")

# %%
ast_size_intervals = (
    ast_sizes
    .groupby(["problem", "step"])
    .agg(
        ast_size_mean=("ast_size", "mean"),
        ast_size_min=("ast_size", "min"),
        ast_size_max=("ast_size", "max"),
        ast_size_median=("ast_size", "median"),
        ast_size_q1=("ast_size", lambda x: np.percentile(x, 25)), 
        ast_size_q3=("ast_size", lambda x: np.percentile(x, 75))
    )
    .reset_index()
)
# %%
ast_size_intervals

# %%
fig = px.line(
    ast_size_intervals,
    x="step",
    y="ast_size_mean",
    facet_row="problem",
    # points=False,
    color_discrete_sequence=["blue"],
    log_y=True,
    labels={
        "ast_size_mean": "AST Sizes",
        "step": "Compilation Step",
    },
    height=1000,
)

fig_upper = px.line(
    ast_size_intervals,
    x='step', 
    y='ast_size_min',
    color_discrete_sequence=["lightblue"],
    log_y=True,
    facet_row="problem",
)
fig.add_traces(fig_upper.data)
fig_lower = px.line(
    ast_size_intervals,
    x='step', 
    y='ast_size_max',
    color_discrete_sequence=["lightblue"],
    log_y=True,
    facet_row="problem",
)
fig_lower.update_traces(fill='tonexty')
fig.add_traces(fig_lower.data)

fig.add_traces(go.Scatter(
    x=[None],
    y=[None], 
    mode='markers',
    marker=dict(size=10, color='blue'),
    legendgroup='group1',
    showlegend=True, 
    name='Mean',
))
fig.add_trace(go.Scatter(
    x=[None], y=[None], mode='markers',
    marker=dict(size=10, color='lightblue'),
    legendgroup='group2', showlegend=True, name='Min/Max Range'
))
fig.update_layout(legend=dict(
    orientation="h",
    yanchor="bottom",
    xanchor="left",
    y=1,
    x=0.25,
))

fig.update_yaxes(matches=None)
fig.write_image("plots/pdf/stack-ast-sizes.pdf")
fig.show()
# %%
(
    ast_sizes
    .groupby(["problem", "step"])
    .agg(
        median_ast_size=("ast_size", "median")
    )
    .reset_index()
    .groupby(["median_ast_size"])
    .agg(
        freq=("median_ast_size", "count")
    )
)
# %%

(
    ast_sizes
    .query("problem == 'number-io'")
    .query("step == 610")
    .agg(
        mean_ast_size=("ast_size", "mean"),
        median_ast_size=("ast_size", "median"),
    )
)
# %%
largest_asts = (
    ast_sizes
    .groupby(["problem", "trial", "gen", "step"])
    .agg(
        max_ast_size=("ast_size", "max")
    )
    .reset_index()
)
# %%
largest_ast_quartiles = (
    largest_asts
    .groupby(["problem", "step"])
    .agg(
        q1_max_ast_size=("max_ast_size", lambda x: np.percentile(x, 25)),
        median_max_ast_size=("max_ast_size", "median"),
        q3_max_ast_size=("max_ast_size", lambda x: np.percentile(x, 75)),
    )
    .reset_index()
)
# %%
fig = px.line(
    largest_ast_quartiles,
    x="step",
    y="median_max_ast_size",
    facet_row="problem",
    # points=False,
    color_discrete_sequence=["blue"],
    # log_y=True,
    labels={
        "median_max_ast_size": "AST Sizes",
        "step": "Compilation Step",
    },
    height=1000,
)

fig_upper = px.line(
    largest_ast_quartiles,
    x='step', 
    y='q1_max_ast_size',
    color_discrete_sequence=["lightblue"],
    # log_y=True,
    facet_row="problem",
)
fig.add_traces(fig_upper.data)
fig_lower = px.line(
    largest_ast_quartiles,
    x='step', 
    y='q3_max_ast_size',
    color_discrete_sequence=["lightblue"],
    # log_y=True,
    facet_row="problem",
)
fig_lower.update_traces(fill='tonexty')
fig.add_traces(fig_lower.data)

fig.add_traces(go.Scatter(
    x=[None],
    y=[None], 
    mode='markers',
    marker=dict(size=10, color='blue'),
    legendgroup='group1',
    showlegend=True, 
    name='Median',
))
fig.add_trace(go.Scatter(
    x=[None], y=[None], mode='markers',
    marker=dict(size=10, color='lightblue'),
    legendgroup='group2', showlegend=True, name='25th/7th Percentile'
))
fig.update_layout(legend=dict(
    orientation="h",
    yanchor="bottom",
    xanchor="left",
    y=1,
    x=0.25,
))

fig.update_yaxes(matches=None)
fig.write_image("plots/pdf/stack-largest-ast-sizes.pdf")
fig.show()
# %%
fig = px.line(
    ast_size_intervals,
    x="step",
    y="ast_size_mean",
    color="problem",
    # log_y=True,
    labels={
        "ast_size_mean": "Mean AST Sizes",
        "step": "Compilation Step",
    },
)
fig.write_image("plots/pdf/stack-mean-ast-sizes.pdf")
fig.show()
# %%
_df = (
    ast_sizes
    .groupby(["problem", "step", "ast_size"])
    .size()
    .reset_index(name="count")
)
_df["log_count"] = np.log10(_df["count"] + 1)
# %%

problems = ['compare-string-lengths', 'count-odds', 'fuel-cost', 'number-io', 'vectors-summed']
fig = make_subplots(rows=5, cols=1, subplot_titles=problems, x_title='Compilation Step', y_title='AST Size')
for i, problem in enumerate(problems):
    focus = _df[_df["problem"] == problem]
    fig.add_trace(
        go.Heatmap(
            x=focus["step"],
            y=focus["ast_size"],
            z=focus["log_count"],
            coloraxis="coloraxis",
            # Maintain original values in hover tooltips
            text=_df["count"],
            hovertemplate='Value: %{text}<extra></extra>',
        ), 
        row=i+1, 
        col=1,
    )
fig.update_layout(height=1000)
fig.update_layout(coloraxis={
    'colorscale': 'viridis', 
    'colorbar': {
        'tickvals': [0, 1, 2, 3, 4, 5], 
        'ticktext': ['1', '10', '100', '1k', '10k', '100k'],
        'title': 'Log Frequency'
    }
})
fig.write_image("plots/pdf/ast-size-heatmap-log-freq.pdf")
fig.show()


# %%

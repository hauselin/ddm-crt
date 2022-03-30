# %% load modules

import concurrent.futures
import gc
from concurrent.futures import ProcessPoolExecutor
from pathlib import Path

import matplotlib.pyplot as plt
import pandas as pd

pd.set_option(
    "display.max_rows",
    8,
    "display.max_columns",
    None,
    "display.width",
    None,
    "display.expand_frame_repr",
    True,
    "display.max_colwidth",
    None,
)
# %%

PS = {
    "data": Path("../data/clean/data_fold01.csv"),
    "output_res": Path("../results/params_pyddm_m0"),
    "output_sim": Path("../results/sims_pyddm_m0"),
}
PS["output_res"].mkdir(parents=True, exist_ok=True)
PS["output_sim"].mkdir(parents=True, exist_ok=True)

PM = {"workers": 10}

# %% prepare subjects' data

df = pd.read_csv(PS["data"])

# remove subjects with too few usable trials and missing covariates (shouldn't remove anyone since preprocessing already removed them)
df = df.query("acc.notna()")
ids = df.groupby("id").size().reset_index(name="n").query("n >= 15").id.to_list()
df = df.query("id in @ids")
df["id"].nunique()
df = df.sort_values(["condition", "id"]).reset_index(drop=True)

subjs = list(df["id"].unique())
print(f"Total subjects: {len(subjs)}")

subjs_done = [int(i.stem.split("_")[1]) for i in PS["output_res"].glob("*.csv")]
print(f"Processed subjs: {len(subjs_done)}")

# remove subjects that have already been processed
subjs = list(set(subjs).difference(subjs_done))
n_left = len(subjs)
print(f"Subjects left: {n_left}")
subjs = df.query("id in @subjs").sort_values(["condition", "id"])["id"].unique()

# %%

from pyddm_m0 import fit_model

subjs = subjs[:3]

if __name__ == "__main__":
    with ProcessPoolExecutor(max_workers=PM["workers"]) as executor:
        # submit jobs
        futures = {}
        for s in subjs:
            df1 = df.query("id == @s").reset_index(drop=True)
            futures[executor.submit(fit_model, df1, PS)] = s

        # process completed jobs
        for i, f in enumerate(concurrent.futures.as_completed(futures)):
            n_left -= 1
            s = futures[f]
            msg = f"Finish DDM subject {s} ({n_left} left to process)"
            print(msg)
            try:
                print(f.result())
            except Exception as e:
                print(f"Exception: {e}")
            del futures[f]
            gc.collect()

# %%

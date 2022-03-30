# %% load modules

from pathlib import Path
import pandas as pd
from datetime import datetime
import os

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

# %% load and prepare data

PS = {
    "data": Path("../data/clean/data_fold01.csv"),
    "results": Path("../results_pyddm/"),
}

df = pd.read_csv(PS["data"])
subject = 3
df_subject = df.query("id == @subject").reset_index(drop=True)

# %% load stuff

import ddm.models
from ddm import Model, Sample, Fittable
from ddm.models import LossRobustBIC

from ddm.functions import fit_adjust_model, hit_boundary
from ddm.models import (
    DriftConstant,
    BoundConstant,
    OverlayNonDecision,
)
from ddm.models.ic import ICPoint
from ddm.functions import fit_adjust_model, display_model
from ddm.plot import plot_fit_diagnostics
import matplotlib.pyplot as plt

# %%


def fit_model(df_subject):
    print(f"Process id: {os.getpid()}")

    # prepare data for ddm
    cols_ddm = ["acc", "rt"]
    cols_id = [
        "id",
        "country",
        "accimp",
        "conservative",
        "age",
        "gender",
        "condition",
        "crt_acc",
        "country_long",
        "female",
        "college",
        "fold",
    ]
    df_ddm = df_subject[cols_ddm].copy()
    df_ddm = df_ddm.dropna().reset_index(drop=True)

    msg = f"Prepare DDM data for subj {df_subject['id'][0]}"
    print(msg)

    # prepare data row to save results later
    res = df_subject[cols_id].loc[:0, :].reset_index(drop=True).copy()
    res["rt"] = df_ddm["rt"].mean()
    res["acc"] = df_ddm["acc"].mean()
    res["rt0"] = df_ddm.query("acc == 0").mean()["rt"]
    res["rt1"] = df_ddm.query("acc == 1").mean()["rt"]

    model_name = f"condition-{res['condition'][0]:02}_country-{res['country'][0]:02}_id-{res['id'][0]:05}"

    # rescale feature
    # col = "perc_acc"
    # colnew = col + "S"
    # df_ddm[colnew] = df_ddm[col] - df_ddm[col].mean()

    # convert to ddm object
    samp = Sample.from_pandas_dataframe(
        df_ddm, rt_column_name="rt", correct_column_name="acc"
    )
    rts = samp.to_pandas_dataframe()["RT"]
    rtmax = rts.max() + 2  # for model fitting

    # define and fit model
    print(f"Fit model: {model_name}")
    driftlimit = 30
    boundlimit = 0.2
    retry = True
    max_tries = 30
    i = 0  # counter to keep track of total attempts
    while retry and i < max_tries:
        i += 1
        boundlimit += 0.10
        print(f"Bound limit: {boundlimit}")

        # specify model
        # noise (SD) is default is 1.0
        model_fit = Model(
            name=model_name,
            drift=DriftConstant(drift=Fittable(minval=-driftlimit, maxval=driftlimit)),
            bound=BoundConstant(B=Fittable(minval=boundlimit, maxval=30)),
            IC=ICPoint(x0=Fittable(minval=-boundlimit, maxval=boundlimit)),
            overlay=OverlayNonDecision(nondectime=Fittable(minval=0.005, maxval=rtmax)),
            dx=0.001,
            dt=0.01,
            T_dur=rtmax,
        )

        # fit model
        fit_adjust_model(samp, model_fit, lossfunction=LossRobustBIC, verbose=False)
        retry = hit_boundary(model_fit)
        if retry and i < max_tries:
            print("Near parameter boundary. Refitting")

    # display_model(model_fit)
    msg = "success"
    if retry:
        msg = "parameter near boundary"
        print("PARAMETER NEAR BOUNDARY!")

    print("Save parameters")
    # save parameters in dataframe
    res["time_fitted"] = datetime.now()
    res["result"] = msg
    res["model_repr"] = repr(model_fit)
    loss = model_fit.fitresult.value()
    res["loss"] = loss
    n = model_fit.get_model_parameter_names()
    p = model_fit.get_model_parameters()
    for ni, pi in zip(n, p):
        res[ni] = pi

    # plot_fit_diagnostics (model=model_fit, sample=samp)
    # simulate data
    print("Simulate data")
    df_ddm["rt_pred"] = 0.0
    df_ddm["acc_pred"] = 0.0
    for r in df_ddm.itertuples():
        s = model_fit.solve()
        pred = s.resample(k=2000)
        # plot_fit_diagnostics(model_fit, pred)
        df_pred = pred.to_pandas_dataframe(drop_undecided=True)
        m = df_pred.mean()
        df_ddm.loc[r.Index, "rt_pred"] = m["RT"]
        df_ddm.loc[r.Index, "acc_pred"] = m["correct"]

    # df_ddm.corr()
    # df_ddm.plot("rt", "rt_pred", kind="scatter")

    # compute simulated means and save results
    print("Save results")
    res["rt_pred"] = df_ddm["rt_pred"].mean()
    res["acc_pred"] = df_ddm["acc_pred"].mean()

    rts = df_ddm.groupby("acc").mean()["rt_pred"]
    res["rt0_pred"] = df_ddm.query("acc == 0").mean()["rt_pred"]
    res["rt1_pred"] = df_ddm.query("acc == 1").mean()["rt_pred"]
    res

    df_ddm["condition"] = res["condition"][0]
    df_ddm["country"] = res["country"][0]
    df_ddm["id"] = res["id"][0]
    df_ddm

    return res, df_ddm


#%%

# r1, r2 = fit_model(df_subject)
# r2

#%%

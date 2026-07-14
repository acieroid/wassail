#!/usr/bin/env python3
from __future__ import annotations

import argparse
import hashlib
import html
import json
import math
import re
import statistics
from pathlib import Path

from runbench import EngineVariant, RunResult, summarize, variant_key


def load_payload(path: Path) -> tuple[dict[str, object], list[RunResult], list[EngineVariant]]:
    payload = json.loads(path.read_text(encoding="utf-8"))
    if not isinstance(payload, dict):
        raise SystemExit(f"invalid payload in {path}: expected object")

    meta = payload.get("meta")
    raw_results = payload.get("results")
    if not isinstance(meta, dict) or not isinstance(raw_results, list):
        raise SystemExit(f"invalid payload in {path}: missing meta/results")

    results = [RunResult(**item) for item in raw_results]

    raw_variants = meta.get("variants", [])
    variants: list[EngineVariant] = []
    if isinstance(raw_variants, list):
        for item in raw_variants:
            if not isinstance(item, dict):
                continue
            try:
                variants.append(EngineVariant(**item))
            except TypeError:
                continue

    if not variants:
        seen: set[str] = set()
        for r in results:
            key = variant_key(engine=r.engine, runtime=r.runtime, mode=r.mode, label=r.label)
            if key in seen:
                continue
            seen.add(key)
            variants.append(EngineVariant(engine=r.engine, runtime=r.runtime, mode=r.mode, bin="", label=r.label))

    return meta, results, variants


def sanitize_artifact_name(rel: str) -> str:
    s = rel.replace("\\", "/")
    s = s.replace("/", "__")
    s = re.sub(r"[^A-Za-z0-9._-]+", "_", s)
    s = s.strip("._-")
    if len(s) <= 160:
        return s
    h = hashlib.sha256(rel.encode("utf-8", errors="strict")).hexdigest()[:12]
    return s[:140] + "_" + h


def choose_baseline(variants: list[EngineVariant], requested: str) -> str:
    if requested:
        return requested
    pref = "wasm3:int:full"
    if any(v.key == pref for v in variants):
        return pref
    if variants:
        return variants[0].key
    raise SystemExit("no variants available in results payload")


def main(argv: list[str]) -> int:
    ap = argparse.ArgumentParser(description="Render plots from an existing u2bench results.json")
    ap.add_argument("--in", dest="infile", required=True, help="input results.json path")
    ap.add_argument("--baseline", default="", help="baseline key override")
    ap.add_argument(
        "--metric",
        choices=["wall", "internal", "auto"],
        default="auto",
        help="metric for summary/ratios/plots; default uses the metric stored in the JSON when present",
    )
    ap.add_argument("--plot", action="store_true", help="render the summary ratio chart")
    ap.add_argument("--plot-out", default="logs/plot.png")
    ap.add_argument("--plot-per-wasm", action="store_true", help="render one plot per wasm benchmark")
    ap.add_argument("--plot-dir", default="logs/plots")
    args = ap.parse_args(argv)

    try:
        import matplotlib.pyplot as plt  # type: ignore[import-not-found]
        from matplotlib.patches import Patch  # type: ignore[import-not-found]
    except Exception as e:
        raise SystemExit(f"matplotlib is required for plotting: {e}")

    meta, results, variants = load_payload(Path(args.infile))
    metric = args.metric
    if metric == "auto":
        saved_metric = meta.get("metric")
        if isinstance(saved_metric, str) and saved_metric in {"wall", "internal", "auto"}:
            metric = saved_metric

    baseline = choose_baseline(variants, args.baseline or str(meta.get("baseline", "")))
    if not any(v.key == baseline for v in variants):
        raise SystemExit(f"baseline not present in variants: {baseline}")

    summ = summarize(results, baseline, metric=metric)
    metric_label = metric
    if metric == "auto":
        metric_label = "auto (internal preferred)"

    if args.plot:
        ratios = summ["ratios_vs_baseline"]  # type: ignore[assignment]
        labels: list[str] = []
        values: list[float] = []
        for v in variants:
            if v.key == baseline:
                continue
            rv = ratios.get(v.key)
            if not rv:
                continue
            val = float(rv["ratio_geomean"])
            if math.isfinite(val) and val > 0:
                labels.append(v.key)
                values.append(val)

        if labels:
            order = sorted(range(len(labels)), key=lambda i: values[i])
            labels = [labels[i] for i in order]
            values = [values[i] for i in order]

            fig_h = max(4.0, 0.35 * len(labels) + 1.0)
            fig, ax = plt.subplots(figsize=(12, fig_h))
            ax.barh(labels, values)
            ax.axvline(1.0, color="black", linewidth=1.0)
            ax.set_xlabel(f"geomean {metric_label} ratio vs baseline ({baseline})")
            ax.set_title(f"u2bench ratios ({metric_label}, lower is faster)")
            ax.invert_yaxis()
            fig.tight_layout()

            plot_out = Path(args.plot_out)
            plot_out.parent.mkdir(parents=True, exist_ok=True)
            fig.savefig(plot_out)
            print(f"plot: {plot_out}")
        else:
            print("plot skipped: no comparable ratios")

    if args.plot_per_wasm:
        plot_root = Path(args.plot_dir)
        plot_root.mkdir(parents=True, exist_ok=True)

        by_wasm: dict[str, list[RunResult]] = {}
        for r in results:
            by_wasm.setdefault(r.wasm, []).append(r)

        kind_to_items: dict[str, list[tuple[str, str, list[str], str]]] = {}

        for wasm_rel, rs in sorted(by_wasm.items()):
            bench_kind = rs[0].bench_kind if rs else "unknown"
            bench_tags = rs[0].bench_tags if rs else []

            per_key: dict[str, list[RunResult]] = {}
            for r in rs:
                key = variant_key(engine=r.engine, runtime=r.runtime, mode=r.mode, label=r.label)
                per_key.setdefault(key, []).append(r)

            bars: list[tuple[str, float, str]] = []
            for key, items in sorted(per_key.items()):
                vals: list[float] = []
                kinds: set[str] = set()
                for r in items:
                    if not r.ok:
                        continue
                    if r.metric_ms is None or not (r.metric_ms > 0.0 and math.isfinite(r.metric_ms)):
                        continue
                    vals.append(float(r.metric_ms))
                    kinds.add(r.metric_kind)
                if not vals:
                    continue
                v = float(statistics.median(vals))
                k = next(iter(kinds)) if len(kinds) == 1 else "mixed"
                bars.append((key, v, k))

            if not bars:
                continue

            bars.sort(key=lambda t: t[1])
            labels = [b[0] for b in bars]
            values = [b[1] for b in bars]
            kinds = [b[2] for b in bars]

            colors: list[str] = []
            for k in kinds:
                if k == "internal":
                    colors.append("tab:blue")
                elif k == "wall":
                    colors.append("tab:red")
                else:
                    colors.append("tab:purple")

            fig_h = max(3.0, 0.35 * len(labels) + 1.2)
            fig, ax = plt.subplots(figsize=(12, fig_h))
            ax.barh(labels, values, color=colors)
            ax.set_xlabel(f"{metric} time (ms) — blue=internal, red=wall")
            ax.set_title(f"{wasm_rel}  [{bench_kind}]")
            ax.invert_yaxis()

            handles: list[Patch] = []
            if "tab:blue" in colors:
                handles.append(Patch(color="tab:blue", label="internal (guest Time/Elapsed)"))
            if "tab:red" in colors:
                handles.append(Patch(color="tab:red", label="wall (harness)"))
            if "tab:purple" in colors:
                handles.append(Patch(color="tab:purple", label="mixed"))
            if handles:
                ax.legend(handles=handles, loc="lower right")

            fig.tight_layout()

            out_dir = plot_root / bench_kind
            out_dir.mkdir(parents=True, exist_ok=True)
            img_name = sanitize_artifact_name(wasm_rel) + ".png"
            img_path = out_dir / img_name
            fig.savefig(img_path)
            plt.close(fig)

            rel_img = f"{bench_kind}/{img_name}"
            kind_to_items.setdefault(bench_kind, []).append((wasm_rel, rel_img, bench_tags, metric))

        idx = plot_root / "index.html"
        parts: list[str] = []
        parts.append("<!doctype html>")
        parts.append('<html><head><meta charset="utf-8"><title>u2bench plots</title></head><body>')
        parts.append("<h1>u2bench per-benchmark plots</h1>")
        parts.append(
            "<p>Bar color encodes the <code>metric_kind</code> used for that result: "
            "<span style='color:#1f77b4'>blue=internal</span>, <span style='color:#d62728'>red=wall</span>.</p>"
        )
        parts.append(f"<p>metric: <code>{html.escape(metric)}</code></p>")

        for bench_kind in sorted(kind_to_items.keys()):
            parts.append(f"<h2>{html.escape(bench_kind)}</h2>")
            parts.append("<ul>")
            for wasm_rel, rel_img, bench_tags, _m in sorted(kind_to_items[bench_kind], key=lambda t: t[0]):
                tag_s = ", ".join(bench_tags)
                parts.append(
                    "<li>"
                    f"<a href=\"{html.escape(rel_img)}\">{html.escape(wasm_rel)}</a>"
                    f" <small>({html.escape(tag_s)})</small>"
                    "</li>"
                )
            parts.append("</ul>")

        parts.append("</body></html>")
        idx.write_text("\n".join(parts) + "\n", encoding="utf-8")
        print(f"plot-per-wasm: {idx}")

    return 0


if __name__ == "__main__":
    raise SystemExit(main(__import__("sys").argv[1:]))

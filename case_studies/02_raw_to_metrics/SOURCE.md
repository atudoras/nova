# Source — study 02 (not yet started)

No dataset chosen. When one is, this file records the URL, licence, download date and
SHA-256 before any analysis runs — see
[`../01_epa_dnt_ontogeny/SOURCE.md`](../01_epa_dnt_ontogeny/SOURCE.md) for the shape.

**The question.** Study 01 begins with a metrics table, which is what NOVA reads. This one
begins with spike times and asks whether the extraction step — firing rate, active
electrodes, ISI bursts, network bursts, STTC synchrony — belongs inside NOVA or stays
upstream in Axion AxIS / `meaRtools` / spikeinterface.

**Open before planning:** `meaRtools` is archived on CRAN and unverified against R 4.4.2.
Any extraction wrapper stays experimental and outside the package build until its output is
checked against values the source dataset reports independently — numbers nobody has
verified are worse than no numbers, because everything downstream inherits them silently.

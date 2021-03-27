# SSP-Bridge

A library with facade executables to convert an output from Tracking_bigraph.

It can normalize a TTS system and transform it into a state space accpeted by SSP library. Tools provided by this package are also capable of dynamically generating simple programs for generating behavior policies using SSP library.

## Usage

Having exported results from `Tracking_bigraph` library, use either all of the executables from the ``bin`` folder except the `all_in_one.exe` (as it was shown in ``/examples/ex1`` folder) or just the `all_in_one.exe` (as in ``/examples/ex2``).

The first method allows for easier debugging if anything goes wrong (by saving all partial results and knowing exactly at which step an error has happened). The second method is faster (it does keep all of the partial results in memory and does certain operations only once since it can keep results until the very end) but more error-prone.

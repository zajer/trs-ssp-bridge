(lang dune 2.7)
(name SSP_bridge)
(version 1.0.0)
(generate_opam_files true)

(source (github zajer/trs-ssp-bridge))
(license MIT)
(authors "Piotr Cybulski")
(maintainers "pikus3@list.pl")
(homepage "https://github.com/zajer/trs-ssp-bridge")
(bug_reports "https://github.com/zajer/trs-ssp-bridge")

(package
  (name SSP_bridge)
  (synopsis "Tools for converting outputs of Tracking_bigraph library into programs using SSP library.")
  (description 
    "A library with facade executables to convert an output from Tracking_bigraph. 
    It can normalize a TTS system and transform it into a state space accpeted by SSP library.
    Tools provided by this package are also capable of dynamically generating simple programs for generating behavior policies using SSP library."
  )
  (tags ("ssp" "tracking_bigraph" "bridge" "conversion"))
  (depends
    (dune (and :build (>= 2.0.0)))
    (odoc :with-doc)
    bigraph
    tracking_bigraph
    csv
    ssp
    (ounit2 :with-test)
  )
)
  

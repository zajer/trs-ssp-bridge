(lang dune 2.7)
(name TRS_bridge)
(version 0.8.0)
(generate_opam_files true)

(source (github zajer/trs-ssp-bridge))
(license MIT)
(authors "Piotr Cybulski")
(maintainers "pikus3@list.pl")
(homepage "https://github.com/zajer/trs-ssb-bridge")
(bug_reports "https://github.com/zajer/trs-ssb-bridge")

(package
  (name TRS_bridge)
  (synopsis "Summary")
  (description "A description")
  (tags ("yolo" "swag"))
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
  

module Normalize
    (normalize,desugar,foldConstants)
where
import Normalize.Desugar
import Normalize.FoldConstants

normalize = foldConstants . desugar

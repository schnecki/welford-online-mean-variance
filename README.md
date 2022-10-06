# Welford: Online mean and variance computation


Example

    example :: [Double] -> IO ()
    example vals = do
      let n = fromIntegral (length vals)
      mean = VB.sum vals / n
      var = VB.sum (VB.map (\x -> (x - mean) ^ 2) vals) / (n - 1)
      (wMean, _, wVarSample) = finalize $ foldl' addValue WelfordExistingAggregateEmpty (VB.toList vals)
      print (mean, var)
      print (wMean, wVarSample)


`WelfordExistingAggregate` is used to save the state. Use the function `finalize` to retrieve the current estimates for the mean, variance and sample variance.

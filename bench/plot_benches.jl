using Pkg
Pkg.add("CSV")
Pkg.add("DataFrames")
Pkg.add("Plots")
using CSV
using DataFrames
# using Statistics
# using StatsPlots
using Plots

function splitup(name)::Tuple{String,String,UInt}
  parts = split(name, '.')
  return (parts[end-2], parts[end-1], parse(UInt, parts[end]))
end

benches = 
  "benches2.csv" |> CSV.File |> DataFrame |> frame -> select(frame,
              :Name => ByRow(splitup) => [:Benchmark, :DataStructure, :Size],
              :"Mean (ps)" => ByRow(x -> x / 10^3) => :Mean,
            )

for bench in unique(benches.Benchmark)
  data = benches[benches.Benchmark .== bench, :]
  p = plot(xlabel="Hashmap size (No. of elements)", ylabel="Mean cputime (ns)", title=bench, palette=:twelvebitrainbow, xminorgrid=true, yminorgrid=true)
  for name in unique(data.DataStructure)
      current_data = data[data.DataStructure .== name, :]
      plot!(p, current_data.Size, current_data.Mean, label=name, add_marker=true)
  end
  savefig(p, bench * ".svg")

  plog = plot(xlabel="Hashmap size (No. of elements)", ylabel="Mean cputime (ns)", title=bench, palette=:twelvebitrainbow, yaxis=:log10, xaxis=:log2, xlims=(1, Inf), xminorgrid=true, yminorgrid=true)
  for name in unique(data.DataStructure)
      current_data = data[data.DataStructure .== name, :]
      plot!(plog, current_data.Size, current_data.Mean, label=name, add_marker=true)
  end
  savefig(plog, bench * "_loglog.svg")
end

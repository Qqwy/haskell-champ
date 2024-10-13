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
  "insert_bench.csv" |> CSV.File |> DataFrame |> frame -> select(frame,
              :Name => ByRow(splitup) => [:Benchmark, :DataStructure, :Size],
              :"Mean (ps)" => ByRow(x -> x / 10^3) => :Mean,
            )

for bench in unique(benches.Benchmark)
  plot(xlabel="Hashmap size (No. of elements)", ylabel="Mean cputime (ns)", title=bench, palette=:twelvebitrainbow, xminorgrid=true, yminorgrid=true)
  for name in unique(benches.DataStructure)
      current_data = benches[benches.DataStructure .== name, :]
      plot!(current_data.Size, current_data.Mean, label=name, add_marker=true)
  end
  savefig(bench * ".svg")

  plot(xlabel="Hashmap size (No. of elements)", ylabel="Mean cputime (ns)", title=bench, palette=:twelvebitrainbow, yaxis=:log10, xaxis=:log2, xlims=(1, 1024), xminorgrid=true, yminorgrid=true)
  for name in unique(benches.DataStructure)
      current_data = benches[benches.DataStructure .== name, :]
      plot!(current_data.Size, current_data.Mean, label=name, add_marker=true)
  end
  savefig(bench * "_loglog.svg")
end

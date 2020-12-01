defmodule AoC01 do
  @doc ~S"""
  Maybe finds two numbers from a list that give `target` in sum and multiplies them

  ## Examples
      iex> AoC01.solve(20, [])
      {:nothing}
      iex> AoC01.solve(20, [1,1,2,3])
      {:nothing}
      iex> AoC01.solve(20,[10,2,3,3,4,4,4,10])
      {:just, 100}
      iex> AoC01.solves(2, 20, [10,10])
      {:just, 100}
      iex> AoC01.solves(3, 20, [10,5,5])
      {:just, 250}
      iex> AoC01.solves(10, 10, [1,1,1,1])
      {:nothing}
  """
  import Util
  def complements(target, x, xs), do: Enum.filter(xs, fn y -> y == target - x end)
  def solve(_ ,[]), do: nothing()
  def solve(target, [x|xs]), do: (cond do
     complements(target, x, xs) |> null -> solve(target, xs)
     true                              -> just(x * hd(complements(target, x, xs)))
  end)

  def solves(_, _, []), do: nothing()
  def solves(count, target, [x|xs]), do: (cond do
    count < 2  -> nothing()
    count == 2 -> solve(target, [x|xs])
    true       -> case solves((count - 1), (target - x), xs) do
      {:nothing} -> solves(count, target, xs)
      {:just, val} -> just(x*val)
    end
  end)
end

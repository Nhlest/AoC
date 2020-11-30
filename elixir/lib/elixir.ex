defmodule Pepega do
  @doc ~S"""
  Sorts list using quicksort algorithm

  ## Examples
      iex> Pepega.sample_function([])
      []
      iex> Pepega.sample_function([5,4,3,2,1])
      [1,2,3,4,5]
  """
  def sample_function([]), do: []
  def sample_function([x|[]]), do: [x]
  def sample_function([x|xs]) do
    lower = Enum.filter(xs, fn(n) -> n < x  end)
    higher = Enum.filter(xs, fn(n) -> n > x  end)
    sample_function(lower) ++  [x] ++ sample_function(higher)
  end
end

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
    sample_function(for n <- xs, n < x , do: n)
    ++ [x] ++
    sample_function(for n <- xs, n >= x, do: n)
  end
  def pepega(), do: fn x -> fn y -> x+y end end
end

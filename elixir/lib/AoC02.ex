defmodule AoC02 do
  @doc ~S"""
  Validates a set of passwords according to the ruleset

  ## Examples
      iex> AoC02.solve([{[lower: 1, higher: 2, char: 'a'], 'asdfa'}])
      1
      iex> AoC02.solve(AoC02.parse(["1-2 a: asdfa", "1-2 b: aaaa", "1-1 c: cccc"]))
      1
      iex> AoC02.solves(AoC02.parse(["1-2 a: asdfa", "1-2 b: aaaa", "1-1 c: cccc"]))
      1
  """
  def ord(c) do
    [{answ, _}] = Enum.with_index(c)
    answ
  end

  def parse([]), do: []
  def parse([x|xs]) do
    [bounds, char, pw] = String.split(x, " ")
    [{lower, _}, {higher, _}] = Enum.map(String.split(bounds, "-"), fn bnd -> Integer.parse(bnd) end)
    [char, ""] = String.split(char, ":")
    [{[lower: lower, higher: higher, char: char], pw} | parse(xs)]
  end

  def solve([]), do: 0
  def solve([x|xs]), do: isValid(x) + solve(xs)

  def isValid({[lower: l, higher: h, char: c], pw}) do
    len = Enum.filter(to_charlist(pw), fn ch -> ch == ord(to_charlist(c)) end) |> length
    if len >= l && len <= h do 1 else 0 end
  end

  def solves([]), do: 0
  def solves([x|xs]), do: isValids(x) + solve(xs)

  def xor(true, true), do: false
  def xor(false, true), do: true
  def xor(true, false), do: true
  def xor(false, false), do: false

  def isValids({[lower: l, higher: h, char: c], pw}) do
    if xor((String.at(pw, l-1) == c), (String.at(pw, h-1) == c)) do 1 else 0 end
  end
end

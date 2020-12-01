defmodule Util do
  def nothing(), do: {:nothing}
  def just(val), do: {:just, val}
  def from_maybe(default, {:nothing}), do: default
  def from_maybe(_default, {:just, val}), do: val
  def null([]), do: true
  def null(_), do: false
end

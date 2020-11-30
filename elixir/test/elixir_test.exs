defmodule PepegaTest do
  use ExUnit.Case
  doctest Pepega

  test "Edge cases" do
    assert Pepega.sample_function([]) == []
    assert Pepega.sample_function([0]) == [0]
  end

  test "Normal cases" do
    assert Pepega.sample_function([5,4,3,2,1]) == [1,2,3,4,5]
    assert Pepega.sample_function([12478,512578,58]) == [58,12478,512578]
  end
end

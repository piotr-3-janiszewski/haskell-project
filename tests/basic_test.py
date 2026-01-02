from test_utils import *

def test_positive_basic():
	assert "Yes" == run_checkreg("a*")

def test_negative_basic():
	assert "No" == run_checkreg("a")

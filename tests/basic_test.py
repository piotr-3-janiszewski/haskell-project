from test_utils import *

def test_positive_basic():
	assert checkreg("a*")

def test_negative_basic():
	assert not checkreg("a")

import subprocess

def run_checkreg(regex):
	result = subprocess.run(['./checkreg', regex], capture_output=True, text=True)
	return result.stdout.strip()

def checkreg(regex):
	result = run_checkreg(regex)

	if "Doesn't" in result:
		return False
	
	return True

import subprocess

def run_checkreg(regex):
	result = subprocess.run(['./checkreg', regex], capture_output=True, text=True)
	return result.stdout.strip()

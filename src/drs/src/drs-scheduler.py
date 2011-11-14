#!/usr/bin/python

# Copyright 2011 MIT Haystack Observatory
# 
# This file is part of Mark6.
# 
# Mark6 is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, version 2 of the License.
# 
# Mark6 is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with Mark6.  If not, see <http://www.gnu.org/licenses/>.

import subprocess
import time
import sys
import optparse
from string import ljust
from xml.etree.ElementTree import ElementTree
import datetime

class Session:

	polling_interval = 0.1

	def __init__(self, scan_list):
		self._scan_list = scan_list

	def execute(self, dryrun=False):
		for s in self._scan_list:
			if s.late():
				print ljust('late scan:', 20),
				print time.time(), s
				print '\n'
				continue

			print ljust('running scan:', 20), s
			print ljust('scan start:', 20),
			print time.ctime(s._start_time)

			while time.time() < s._start_time:
				time.sleep(Session.polling_interval)

			print ljust('now/start_time:', 20),
			print time.time(), s._start_time

			start_time = time.time()
			if not dryrun:
				p = subprocess.Popen(s.args())
				p.wait()	
			else:
				time.sleep(s._duration)

			duration = time.time() - start_time
			print ljust('scan/actual duration:', 20), s._duration, duration
			print '\n'
			
class Scan:

	mark6_exec = '/opt/mit/mark6/bin/net2raid-run'

	def __init__(self, experiment_name, source, station_code, start_time,
		     duration, scan_name):
		self._experiment_name = experiment_name
		self._source = source
		self._station_code = station_code
		self._start_time = int(start_time)
		self._duration = int(duration)
		self._end_time = self._start_time + self._duration
		self._scan_name = scan_name

        def __str__(self):
                return ''.join([
                        '<scan ',
                        'experiment="%s" '%self._experiment_name,
                        'source="%s" '%self._source,
                        'station_code="%s" '%self._station_code,
                        'start_time="%d" '%self._start_time,
                        'duration="%d" '%self._duration,
                        'scan_name="%s"'%self._scan_name,
                        '/>'
                        ])

	def args(self):
		return [ Scan.mark6_exec, self._scan_name,
			 str(self._duration) ]

	def late(self):
		if time.time() > self._end_time:
			return True
		return False	


def test():
	print 'Running test schedule.'
	now = time.time()
	experiment_name='TEST'
	TEST_SCANS = [
		('4242', 'Wf', now-5, 43, 'r1504_Wf_297-1700'),
		('4242', 'Wf', now+53, 43, 'r1504_Wf_297-1702'),
		('4243', 'Wf', now+106, 43, 'r1504_Wf_297-1707'),
		('4244', 'Wf', now+159, 43, 'r1504_Wf_297-1711'),
		('4245', 'Wf', now+212, 120,'r1504_Wf_297-1713'),
		]

	scan_list = []
	for s in TEST_SCANS:
		sr, st, t, d, sn = s
		scan_list.append(Scan(experiment_name=experiment_name,
				      source=sr,
				      station_code=st,
				      start_time=t,
				      duration=d,
				      scan_name=sn))

	session = Session(scan_list)
	session.execute()


def run_schedule(schedule, dryrun):
	print 'Running schedule:', schedule
	tree = ElementTree()
	tree.parse(schedule)
	experiment = tree.getroot()
	experiment_name = experiment.attrib['name']
	scans = list(tree.findall('scan'))
	scan_list = []
	for s in scans:
		a = s.attrib
		scan_list.append(Scan(experiment_name=experiment_name,
					source=a['source'],
					station_code=a['station_code'],
					start_time=int(a['start_time']),
					duration=int(a['duration']),
					scan_name=a['scan_name']))
	
	session = Session(scan_list)
	session.execute(dryrun)
	

if __name__ == '__main__':
	parser = optparse.OptionParser()
	parser.add_option('-s', '--schedule', dest='schedule',
			  help='Schedule ')
	parser.add_option('-t', '--test', action='store_true', dest='test',
			  help='Run a test schedule based on current time.',
			  default=False)
	parser.add_option('-d', '--dryrun', action='store_true', dest='dryrun',
			  help='Dryrun of schedule.', default=False)

	(o, a) = parser.parse_args()

	if o.test:
		test()
		sys.exit(0)

	if o.schedule:
		run_schedule(o.schedule, o.dryrun)
		sys.exit(0)

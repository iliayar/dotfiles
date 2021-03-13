#!/usr/bin/env python

from org_agenda import AgendaParser

from datetime import datetime
import subprocess
import os
import stat

FIFO = '/tmp/agenda.io'

def make_fifo():
    global FIFO
    print(f'Making fifo {FIFO}')
    os.mkfifo(FIFO, 0o666)

def notify(event):
    expire_time = str(5*60*1000)
    app_name = 'Org Agenda'
    title = event[AgendaParser.CATEGORY]
    body = '<b>' + event[AgendaParser.TIME] + '</b> ' + event[AgendaParser.TEXT]
    subprocess.run(['notify-send', title, body, '-a', app_name, '-t', expire_time])

if __name__ == '__main__':
    parser = AgendaParser()
    event = next(parser.agenda)
    time = event[AgendaParser.TIME].split('-')[0]

    (hour, minutes) = map(int, time.split(':'))
    time = hour*60 + minutes
    now = datetime.now().hour*60 + datetime.now().minute
    diff = min(abs(time - now), (time + 24*60 - now))
    print(diff)
    if diff <= 10:
        notify(event)

    if os.path.exists(FIFO):
        if not stat.S_ISFIFO(os.stat(FIFO).st_mode):
            make_fifo()
    else:
        make_fifo()

    try:
        while event[AgendaParser.FILE] != 'Study':
            event = next(parser.agenda)
        data = event[AgendaParser.TIME] + " " + event[AgendaParser.TEXT]
    except StopIteration:
        data = 'Chill'

    open(FIFO, 'w').write(data + '\n')

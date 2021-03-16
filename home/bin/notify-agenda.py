#!/usr/bin/env python

from org_agenda import AgendaParser

from datetime import datetime
import subprocess
import os
import stat

FIFO = '/tmp/agenda.io'

def make_fifo():
    global FIFO
    os.mkfifo(FIFO, 0o666)

def notify(event):
    expire_time = str(5*60*1000)
    app_name = 'Org Agenda'
    title = event.type
    body = '<b>' + event.time + '</b> ' + event.text
    subprocess.run(['notify-send', title, body, '-a', app_name, '-t', expire_time])

def notify_work():
    agenda = AgendaParser().get_iter()
    event = None
    for e in agenda:
        if e.time != '':
            event = e
            break

    if event == None:
        return

    time = event.time.split('-')[0]
    (hour, minutes) = map(int, time.split(':'))
    time = hour*60 + minutes
    now = datetime.now().hour*60 + datetime.now().minute
    diff = min(abs(time - now), (time + 24*60 - now))
    if diff <= 10:
        notify(event)

def bar_work():
    agenda = AgendaParser().get_iter()
    if os.path.exists(FIFO):
        if not stat.S_ISFIFO(os.stat(FIFO).st_mode):
            make_fifo()
    else:
        make_fifo()

    event = None
    for e in agenda:
        if e.file == 'Study':
            event = e
            break
    if event == None:
        data = 'Chill'
    else:
        if event.time == '':
            time = 'Whole day'
        else:
            time = event.time
        data = time + ' ' + event.text

    open(FIFO, 'w').write(data + '\n')


if __name__ == '__main__':
    notify_work()
    bar_work()

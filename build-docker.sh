#!/usr/bin/env bash
guix pack --no-grafts -f docker -m manifest.scm -S /bin=bin --entry-point=/bin/fuglesteg.net

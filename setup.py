#!/usr/bin/python
# -*- coding: utf-8 -*-

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

from setuptools import setup

setup(name='colorg',
      version='0.0',
      description="Server for real-time collaborative edition of Org files.",
      author=u"François Pinard",
      author_email='pinard@iro.umontreal.ca',
      url='https://github.com/pinard/colorg',
      install_requires=['gevent'],
      scripts=['colorg-server'],
      license='GPLv3'
)

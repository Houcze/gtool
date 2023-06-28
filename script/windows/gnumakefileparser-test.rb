#!/usr/bin/env ruby
#
#== GNUMakefileParser クラスのチェックスクリプト
#
#Authors::   Yasuhiro MORIKAWA
#Version::   $Id: gnumakefileparser-test.rb,v 1.1 2009-03-25 08:17:36 morikawa Exp $
#Tag Name::  $Name:  $
#Copyright:: Copyright (C) GFD Dennou Club, 2007-. All rights reserved.
#License::   See COPYRIGHT[link:../../COPYRIGHT]
#

# Test::Unit
require "test/unit"
require "gnumakefileparser"

# Color のテストクラス.
class TestGNUMakefileParser < Test::Unit::TestCase

  def setup
    tmpdir = 'tmp' + Process.pid.to_s
    @tmpmkpath1 = File.join(tmpdir, 'Makefile')
    @tmpmkpath2 = File.join(tmpdir, 'depend')
    Dir::mkdir(tmpdir)
    @mkbody1 = <<-EOMK

all: 

clean.all: clean
	-$(RM) $(MODS) work.pc*
	-$(RM) $(MODS) *.o $(TARGET)

TARGET=lib$(LIBNAME).a

OBJS = \
	main.o line.o \
	sysdep-$(SYSDEP).o

include depend

FC ?= g95

F90 := $(FC)

%.o: %.f90

.SUFFIX: .obj .f90

.f90.obj: 
	$(FC) -c $*.f90

EOMK

    @mkbody2 = <<-EOMK
main.o: line.o
line.o: sysdep-$(SYSDEP).o
EOMK

    tmpmkio = File.open(@tmpmkpath1, 'w')
    tmpmkio.puts(@mkbody1)
    tmpmkio.close
    tmpmkio = File.open(@tmpmkpath2, 'w')
    tmpmkio.puts(@mkbody2)
    tmpmkio.close
    @gmkfile = GNUMakefileParser.new(@tmpmkpath1 + ',' + @tmpmkpath2)
    File.delete(@tmpmkpath1)
    File.delete(@tmpmkpath2)
    Dir.rmdir(tmpdir)
  end

  # def teardown
  # end

  #GNUMakefileParser#new のテスト
  def test_s_new
    assert_instance_of(GNUMakefileParser, @gmkfile)
    assert_equal(@tmpmkpath1 + ',' + @tmpmkpath2, @gmkfile.file)
    assert_equal(@mkbody1 + "\n" + @mkbody2 + "\n", @gmkfile.body)
    assert_equal('all: 
clean.all: clean
	-$(RM) $(MODS) work.pc*
	-$(RM) $(MODS) *.o $(TARGET)
TARGET = lib$(LIBNAME).a
OBJS = main.o line.o sysdep-$(SYSDEP).o
include depend
FC ?= g95
F90 := $(FC)
%.o: %.f90
.SUFFIX: .obj .f90
.f90.obj: 
	$(FC) -c $*.f90
main.o: line.o
line.o: sysdep-$(SYSDEP).o
', @gmkfile.to_s)
    assert_equal('TARGET = lib$(LIBNAME).a
OBJS = main.o line.o sysdep-$(SYSDEP).o
include depend
FC ?= g95
F90 := $(FC)
', @gmkfile.to_s(false))
    assert_equal('all: 
clean.all: clean
	-$(RM) $(MODS) work.pc*
	-$(RM) $(MODS) *.o $(TARGET)
TARGET = lib$(LIBNAME).a
OBJS = main.o line.o sysdep-$(SYSDEP).o
FC ?= g95
F90 := $(FC)
%.o: %.f90
.SUFFIX: .obj .f90
.f90.obj: 
	$(FC) -c $*.f90
main.o: line.o
line.o: sysdep-$(SYSDEP).o
', @gmkfile.to_s(true, true, false))


    assert_instance_of(GNUMakefileParser::MkTarget, @gmkfile.list[0])
    assert_equal('all', @gmkfile.list[0].target)
    assert_equal([], @gmkfile.list[0].prerequisites)
    assert_equal([], @gmkfile.list[0].commands)
    assert_equal('all: 
', @gmkfile.list[0].to_s)


    assert_instance_of(GNUMakefileParser::MkTarget, @gmkfile.list[1])
    assert_equal('clean.all', @gmkfile.list[1].target)
    assert_equal(['clean'], @gmkfile.list[1].prerequisites)
    assert_equal(['-$(RM) $(MODS) work.pc*',
                  '-$(RM) $(MODS) *.o $(TARGET)'],
                 @gmkfile.list[1].commands)
    assert_equal('clean.all: clean
	-$(RM) $(MODS) work.pc*
	-$(RM) $(MODS) *.o $(TARGET)
', @gmkfile.list[1].to_s)

    assert_instance_of(GNUMakefileParser::MkVariable, @gmkfile.list[2])
    assert_equal('TARGET', @gmkfile.list[2].var)
    assert_equal('lib$(LIBNAME).a', @gmkfile.list[2].value)
    assert_equal('TARGET = lib$(LIBNAME).a
', @gmkfile.list[2].to_s)

    assert_instance_of(GNUMakefileParser::MkVariable, @gmkfile.list[3])
    assert_equal('OBJS', @gmkfile.list[3].var)
    assert_equal('main.o line.o sysdep-$(SYSDEP).o', @gmkfile.list[3].value)
    assert_equal('OBJS = main.o line.o sysdep-$(SYSDEP).o
', @gmkfile.list[3].to_s)

    assert_instance_of(GNUMakefileParser::MkOther, @gmkfile.list[4])
    assert_equal('include depend', @gmkfile.list[4].line)
    assert_equal('include depend
', @gmkfile.list[4].to_s)

    assert_instance_of(GNUMakefileParser::MkVariable, @gmkfile.list[5])
    assert_equal('FC', @gmkfile.list[5].var)
    assert_equal('g95', @gmkfile.list[5].value)
    assert_equal('', @gmkfile.list[5].simply_expanded)
    assert_equal('?', @gmkfile.list[5].conditional)
    assert_equal('FC ?= g95
', @gmkfile.list[5].to_s)

    assert_instance_of(GNUMakefileParser::MkVariable, @gmkfile.list[6])
    assert_equal('F90', @gmkfile.list[6].var)
    assert_equal('$(FC)', @gmkfile.list[6].value)
    assert_equal(':', @gmkfile.list[6].simply_expanded)
    assert_equal('', @gmkfile.list[6].conditional)
    assert_equal('F90 := $(FC)
', @gmkfile.list[6].to_s)


  end

end

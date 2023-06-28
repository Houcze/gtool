#!/usr/bin/env ruby
#
#== GNU Make の Makefile パーサ
#
#Authors::   Yasuhiro MORIKAWA
#Version::   $Id: gnumakefileparser.rb,v 1.1 2009-03-25 08:17:35 morikawa Exp $
#Tag Name::  $Name:  $
#Copyright:: Copyright (C) GFD Dennou Club, 2007-. All rights reserved.
#License::   See COPYRIGHT[link:../../COPYRIGHT]
#
#
#GNU Make 用 Makefile の解析スクリプト
#
#
class GNUMakefileParser

  attr_reader :file
  attr_reader :body
  attr_accessor :list

  #
  #引数 *file* には Makefile 名または組み込み変数 STDIN を与えます.
  #Makefile を読み込めない場合にはエラーを返します.
  #空文字を与えると, 空の Makefile を与えることと同様になります.
  #複数の Makefile を開く場合には, ファイル名を ',' で区切って
  #与えます.
  #
  #Makefile から読み込まれた内容は解析され, 個々をオブジェクトに変換
  #されて @list に格納されます. GNUMake の全ての要素が解析
  #されるわけでないので注意してください.
  #
  #
  def initialize(file=nil)
    @file = file
    @body = ''
    if @file.class == String
      if !@file.empty?
        files = @file.split(/,/)
        files.each{ |fi|
          @body << File.open(fi, 'r') {|f| f.read}
          @body << "\n"
        }
      end
    elsif @file == STDIN
      @body = STDIN.read || ''
    else
      raise "  argument \"file\" must be String or STDIN"
    end
    @list = []
    @list += self.parse(@body)
  end

  #
  #引数 *body* に与えられた文字列を Makefile 内の要素として解析し,
  #GNUMakefileParser のオブジェクトに追加します.
  #
  def add(body)
    @list += self.parse(body)
  end

  def +(other)
    newmk = self.dup
    newmk.list = @list + other.list
    return newmk
  end

#  def +=(other)
#    @list += other.list
#  end

  #
  #引数 *body* に与えられた文字列を Makefile 内の要素として解析し,
  #個々をオブジェクトに変換して配列として返します.
  #GNUMake の全ての要素を解析
  #できるわけでないので注意してください.
  #
  def parse(body)
    list = []
    body.gsub!(/\s*\\\s*\n/, ' ')
    unit = ''
    kind = nil
    body.split("\n").each{|line|
      if line =~ /^\t/
        unit = unit + "\n" + line
      elsif line =~ /^\s*\#/
        next
      elsif line =~ /^\s*$/
        next
      else
        if kind == :mktarget
          list << MkTarget.new(unit)
          kind = false
        elsif kind == :mkvariable
          list << MkVariable.new(unit)
          kind = false
        end
        if line =~ /^\s*\w+\s*:?\??\=/
          kind = :mkvariable
          unit = line
        elsif line =~ /^[\w\s\-\.\,\$\(\)\%]+:/
          kind = :mktarget
          unit = line
        else
          list << MkOther.new(line)
        end
      end
    }
    if kind == :mktarget
      list << MkTarget.new(unit)
    elsif kind == :mkvariable
      list << MkVariable.new(unit)
    end
    return list
  end

  #
  #オブジェクトの内容を Makefile 用のテキストに変換.
  #
  def to_s(target=true, variable=true, other=true)
    str = ''
    @list.each{|obj|
      if obj.class == GNUMakefileParser::MkTarget
        next if !target
      elsif obj.class == GNUMakefileParser::MkVariable
        next if !variable
      elsif obj.class == GNUMakefileParser::MkOther
        next if !other
      elsif !obj
        next
      end
      str << obj.to_s
    }
    return str
  end


  #
  #下記のように記述される Makefile のターゲット.
  #
  #   test.o: test.f90
  #           f90 $< -o $@
  #
  class MkTarget
    attr_accessor :target
    attr_accessor :prerequisites
    attr_accessor :commands

    #
    #引数 target_rules に上記の書式の文字列を与えることで,
    #それぞれ @tareget, @prerequisites, @command が設定される.
    #
    def initialize(target_rules)
      rule = target_rules.split("\n")[0]
      if rule =~ /^([\w\s\-\.\,\$\(\)\%]+):\s*(.*)\s*$/
        target = $1
        prerequisites = $2
        @target = target.sub(/^\s*/, '').sub(/\s*$/, '')
        @prerequisites = []
        prerequisites.split(/\s+/).each{|i|
          @prerequisites << i
        }
      else
        raise "Syntax Error"
      end
      @commands = []
      target_rules.split("\n")[1..-1].each{|c|
        commands << c.sub(/^\s+/, '').gsub(/\t+/, ' ')
      }
    end

    #
    #オブジェクトの内容を Makefile 用のテキストに変換.
    #
    def to_s
      str = @target + ': ' + @prerequisites.join(' ')
      if @commands.size > 0
        str = str + "\n\t" + @commands.join("\n\t")
      end
      return str + "\n"
    end
  end

  #
  #下記のように記述される変数定義.
  #
  #    objects = main.o command.o display.o \
  #              insert.o search.o
  #
  class MkVariable
    attr_accessor :var
    attr_accessor :value
    attr_accessor :simply_expanded
    attr_accessor :conditional

    #
    #引数 var_rules に上記の書式の文字列を与えることで,
    #それぞれ @var, @value, が設定される.
    #
    def initialize(var_rules)
      if var_rules =~ /^\s*(\w+)\s*(:?)(\??)\=\s*(.*)\s*$/
        @var = $1
        @simply_expanded = $2
        @conditional = $3
        value = $4
        @value = value.gsub(/[\t ]+/, ' ')
      else
        raise "Syntax Error"
      end
    end

    #
    #オブジェクトの内容を Makefile 用のテキストに変換.
    #
    def to_s
      return @var + ' ' + @simply_expanded + @conditional + '= ' + @value + "\n"
    end

  end

  #
  #ターゲットや編集代入ではない行
  #
  class MkOther
    attr_accessor :line

    #
    #引数 line に文字列を与えることで, @line が設定される.
    #
    def initialize(line)
      @line = line
    end

    #
    #オブジェクトの内容を Makefile 用のテキストに変換.
    #
    def to_s
      return @line + "\n"
    end
  end


end


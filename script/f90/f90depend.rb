#!/usr/bin/env ruby
# coding: utf-8
#
#= Fortran 90/95 dependency lister
#
# Authors::   Yasuhiro MORIKAWA
# Version::   $Id: f90depend.rb,v 1.1 2009-03-25 08:17:36 morikawa Exp $
# Tag Name::  $Name:  $
# Copyright:: Copyright (C) GFD Dennou Club, 2006. All rights reserved.
# License::   See COPYRIGHT[link:../../COPYRIGHT]
#
#
#引数として受け取る Fortran 90/95 ファイル内の use 文および
#module 文を解析し, 依存関係を記したファイルを Makefile 形式
#で標準出力に書き出す.
#
require "optparse"

class Fortran90Dependency

  def initialize(quiet=nil, verbose=nil, strictly=nil)
    @entries = []
    @quiet = quiet
    @verbose = verbose
    @strictly = strictly
    @progress = $stderr unless quiet
  end

  #
  #探査するファイルを追加する.
  #引数 files には Fortran90/95 ファイル名を与える.
  #ファイルが存在しない場合, エラーを返す.
  #
  def add_files(*files)
    files.flatten!
    if @strictly
      progress "\n  ** ATTENTION **  Strictly parsing might take a long time. \n"
    end
    progress "  Adding     "
    files.each{|file|
      progress "\n    #{file}", '.'
      entry = Fortran90DependencyEntry.new(file, @strictly)
      progress "\n      Main Program: ", ''
      if entry.mainprogram
        progress "  exist", ''
      else
        progress "  none", ''
      end
      progress "\n      Modules: ", ''
      entry.modules.each{ |m|
        progress "\n        #{m}", ''
      }
      progress "\n      Uses: ", ''
      entry.uses.each{ |u|
        progress "\n        #{u}", ''
      }
      @entries << entry if entry.modules
    }
    progress "\n"
    @entries
  end

  #
  #探査するファイルリストから, ファイルを削除する.
  #引数 files には Fortran90/95 ファイル名を与える.
  #
  def del_files(*files)
    files.flatten!
    progress "  Deleting   "
    files.each{|file|
      @entries.collect!{|entry|
        if File.expand_path(file) == File.expand_path(entry.file)
          progress "\n    #{file}", '.'
          next nil
        else
          next entry
        end
      }
      @entries.delete_if{|entry| !entry }
    }
    progress "\n"
    @entries
  end

  #
  #依存関係リストを作成する
  #
  def gen_list
    dep_list = []
    exec_dep_hash = {}
    progress "  Generating "
    @entries.each{|entry|
      progress "."
      dep_line = entry.file.sub(/\.erb|\.f9(0|5)$/i, '.o') + ': '
      entry.uses.each{|use_item|
        @entries.each{|checked_entry|
          if checked_entry.modules.include?(use_item)
            dep_line << checked_entry.file.sub(/\.erb|\.f9(0|5)$/i, '.o') + ' '
            break
          end
        }
      }
      dep_list << dep_line
      if entry.mainprogram
        mainfile = dep_line.sub(/(.*?)\.o:.*/, '\\1')
        exec_dep_hash[mainfile] = dep_line.sub(/.*?: /, '').split(' ')
      end
    }
    exec_dep_list = []
    exec_dep_hash.each{|main, dep_array|
      exec_dep_obs = dep_array
      dep_array.each{ |edo|
        dep_list.each{ |dl|
          if edo == dl.sub(/:.*/, '')
            exec_dep_obs << dl.sub(/.*?: /, '').split(' ')
            exec_dep_obs.flatten!
            exec_dep_obs.uniq!
          end
        }
      }
      exec_dep_list << main + ": " + exec_dep_obs.join(' ')
    }
    progress "\n"
    (dep_list + exec_dep_list).join("\n")
  end

  private

  #
  # 引数 *char* に与えられたメッセージを表示する.
  # @quiet が true の場合は何も表示しない.
  # 第 2 引数 *clipto* に String クラスが与えられ, 且つ
  # @varbose が true ではない場合, *char* に与えられた文字列は *clipto*
  # に置き換えられます.
  #
  def progress(char, clipto=nil)
    unless @quiet
      char = clipto if clipto && !(@verbose) && clipto.class.to_s == 'String'
      @progress.print(char)
      @progress.flush
    end
  end


  class Fortran90DependencyEntry

    attr_reader :file
    attr_reader :mainprogram
    attr_reader :modules
    attr_reader :uses

    #引数 file には Fortran 90/95 ファイル名を与える.
    #ファイルが存在しない場合, エラーを返す.
    #
    #strictly に true を与えると Fortran 90/95 ファイル内の
    #継続行マーカ "&" や 改行マーカ ";" に関しても正しく
    #解析しますが, 実行時間が大幅に増えます.
    #
    def initialize(file, strictly=nil)
      @file = file
      if !(file =~ /\.erb|\.f9(0|5)$/i)
        $stderr.print "\n  Warning: \"#{file}\" is not recognized as a Fortran 90/95 file.\n" +
          "           Please rename a suffix of this file to .f90 etc.\n"
        return nil
      end

      @strictly = strictly

      body = File.open(@file, 'r') {|f| f.read}
      if @strictly
        body = united_to_one_line(body)
        body = semicolon_to_linefeed(body)
      end
      @modules = find_modules(body)
      @mainprogram = find_mainprogram(body)
      @uses = find_uses(body)
      @uses.collect!{|use_item|
        if @modules.include?(use_item)
          nil
        else
          use_item
        end
      }
      @uses.delete_if{|use_item| !use_item }
    end

    private

    #
    #引数 body で与えられる Fortran90/95 ソースコード内で
    #定義される module 名を取り出す.
    #
    def find_modules(body)
      modules = []
      body.split("\n").each{ |line|
        if line =~ /^\s*?module\s+(\w+)\s*?(!.*?)?$/i
          modules << $1
        end
      }
      return modules
    end

    #
    #引数 body で与えられる Fortran90/95 ソースコード内で
    #定義される use 文を取り出す.
    #
    def find_uses(body)
      uses = []
      body.split("\n").each{ |line|
        if line =~ /^\s*?use\s+(\w+)(.*?)(!.*?)?$/i
          uses << $1
        end
      }
      return uses.uniq
    end

    #
    #引数 body で与えられる Fortran90/95 ソースコード内で
    #主プログラムが定義されていれば true を, 定義されて
    #いない場合には false を返す
    #
    def find_mainprogram(body)
      other_block_level_depth = 0
      other_block_searching_flag = false
      body.split("\n").each{ |line|
        if !other_block_searching_flag
          if line =~ /^\s*?program\s+(\w+)\s*?(!.*?)?$/i ||
              line =~ /^\s*?\w/ && !block_start?(line, @strictly)
            return true
          elsif block_start?(line, @strictly)
            other_block_searching_flag = true
            next
          else
            next
          end
        else
          other_block_level_depth += 1 if block_start?(line, @strictly)
          other_block_level_depth -= 1 if block_end?(line)
          if other_block_level_depth < 0
            other_block_level_depth = 0
            other_block_searching_flag = false
          end
          next
        end
      }
      return false
    end

    #
    # {RDoc Fortran 90/95 解析機能強化版}[http://www.gfd-dennou.org/library/dcmodel]
    # の Fortran 95 パーサから移植したメソッド.
    #
    # Which "line" is start of block (module, program, block data,
    # subroutine, function) statement ?
    #
    def block_start?(line, strictly=nil)
      return nil if !line

      if strictly
        if line =~ /^\s*?module\s+(\w+)\s*?(!.*?)?$/i    ||
            line =~ /^\s*?program\s+(\w+)\s*?(!.*?)?$/i  ||
            line =~ /^\s*?block\s+data(\s+\w+)?\s*?(!.*?)?$/i     ||
            line =~ \
                    /^\s*?
                     (recursive|pure|elemental)?\s*?
                     subroutine\s+(\w+)\s*?(\(.*?\))?\s*?(!.*?)?$
                    /ix ||
            line =~ \
                    /^\s*?
                     (recursive|pure|elemental)?\s*?
                     (
                         character\s*?(\([\w\s\=\(\)\*]+?\))?\s+
                       | type\s*?\([\w\s]+?\)\s+
                       | integer\s*?(\([\w\s\=\(\)\*]+?\))?\s+
                       | real\s*?(\([\w\s\=\(\)\*]+?\))?\s+
                       | double\s+precision\s+
                       | logical\s*?(\([\w\s\=\(\)\*]+?\))?\s+
                       | complex\s*?(\([\w\s\=\(\)\*]+?\))?\s+
                     )?
                     function\s+(\w+)\s*?
                     (\(.*?\))?(\s+result\((.*?)\))?\s*?(!.*?)?$
                    /ix
          return true
        end
      else
        if line =~ /^\s*?module\s+(\w+)\s*?(!.*?)?$/i    ||
            line =~ /^\s*?program\s+(\w+)\s*?(!.*?)?$/i  ||
            line =~ /^\s*?block\s+data(\s+\w+)?\s*?(!.*?)?$/i     ||
            line =~ \
                    /^\s*?
                     (recursive|pure|elemental)?\s*?
                     subroutine\s+(\w+)
                    /ix ||
            line =~ \
                    /^\s*?
                     (recursive|pure|elemental)?\s*?
                     (
                         character\s*?(\([\w\s\=\(\)\*]+?\))?\s+
                       | type\s*?\([\w\s]+?\)\s+
                       | integer\s*?(\([\w\s\=\(\)\*]+?\))?\s+
                       | real\s*?(\([\w\s\=\(\)\*]+?\))?\s+
                       | double\s+precision\s+
                       | logical\s*?(\([\w\s\=\(\)\*]+?\))?\s+
                       | complex\s*?(\([\w\s\=\(\)\*]+?\))?\s+
                     )?
                     function\s+(\w+)
                    /ix
          return true
        end
      end

      return nil
    end

    #
    # {RDoc Fortran 90/95 解析機能強化版}[http://www.gfd-dennou.org/library/dcmodel]
    # の Fortran 95 パーサから移植したメソッド.
    #
    # Which "line" is end of block (module, program, block data,
    # subroutine, function) statement ?
    #
    def block_end?(line)
      return nil if !line

      if line =~ /^\s*?end\s*?(!.*?)?$/i                 ||
          line =~ /^\s*?end\s+module(\s+\w+)?\s*?(!.*?)?$/i       ||
          line =~ /^\s*?end\s+program(\s+\w+)?\s*?(!.*?)?$/i      ||
          line =~ /^\s*?end\s+block\s+data(\s+\w+)?\s*?(!.*?)?$/i  ||
          line =~ /^\s*?end\s+subroutine(\s+\w+)?\s*?(!.*?)?$/i   ||
          line =~ /^\s*?end\s+function(\s+\w+)?\s*?(!.*?)?$/i
        return true
      end

      return nil
    end


    #
    # {RDoc Fortran 90/95 解析機能強化版}[http://www.gfd-dennou.org/library/dcmodel]
    # の Fortran 95 パーサから移植したメソッド.
    #
    #
    # Semicolons are replaced to line feed.
    #
    def semicolon_to_linefeed(text)
      return "" unless text
      lines = text.split("\n")
      lines.collect!{ |line|
        indent_space = ""
        if line =~ /^(\s+)/
          indent_space = $1
        end
        words = line.split("")
        commentout = false
        squote = false ; dquote = false
        words.collect! { |char|
          if !(squote) && !(dquote) && !(commentout)
            case char
            when "!" ; commentout = true ; next char
            when "\""; dquote = true     ; next char
            when "\'"; squote = true     ; next char
            when ";" ;                     "\n"+indent_space
            else next char
            end
          elsif commentout
            next char
          elsif squote
            case char
            when "\'"; squote = false ; next char
            else next char
            end
          elsif dquote
            case char
            when "\""; dquote = false ; next char
            else next char
            end
          end
        }
        words.join("")
      }
      return lines.join("\n")
    end

    #
    # {RDoc Fortran 90/95 解析機能強化版}[http://www.gfd-dennou.org/library/dcmodel]
    # の Fortran 95 パーサから移植したメソッド.
    #
    # Continuous lines are united.
    #
    # Comments in continuous lines are removed.
    # If delete_space=false, spaces around "&" are not deleted.
    #
    # Example
    #
    # before
    #
    #    subroutine func(a, b, c, d, e, & ! ignored comments
    #      &             f, g, h)         ! valid comments
    #
    # after
    #
    #    subroutine func(a, b, c, d, e, f, g, h)         ! valid comments
    #
    def united_to_one_line(f90src, delete_space=true)
      return "" unless f90src
      lines = f90src.split("\n")
      previous_continuing = false
      now_continuing = false
      body = ""
      lines.each{ |line|
        words = line.split("")
        next if words.empty? && previous_continuing
        commentout = false
        brank_flag = true ; brank_char = ""
        squote = false    ; dquote = false
        ignore = false
        words.collect! { |char|
          if previous_continuing && brank_flag
            now_continuing = true
            ignore         = true
            case char
            when "!"                       ; break
            when " " ; brank_char << char  ; next ""
            when "&"
              brank_flag = false
              now_continuing = false
              next ""
            else
              brank_flag     = false
              now_continuing = false
              ignore         = false
              next brank_char + char
            end
          end
          ignore = false

          if now_continuing
            next ""
          elsif !(squote) && !(dquote) && !(commentout)
            case char
            when "!" ; commentout = true     ; next char
            when "\""; dquote = true         ; next char
            when "\'"; squote = true         ; next char
            when "&" ; now_continuing = true ; next ""
            else next char
            end
          elsif commentout
            next char
          elsif squote
            case char
            when "\'"; squote = false ; next char
            else next char
            end
          elsif dquote
            case char
            when "\""; dquote = false ; next char
            else next char
            end
          end
        }
        if !ignore && !previous_continuing || !brank_flag
          if previous_continuing
            if delete_space
              joined_words = words.join("")
              body = body.rstrip + " " + joined_words.lstrip
            else
              body << words.join("")
            end
          else
            body << "\n" + words.join("")
          end
        end
        previous_continuing = now_continuing ? true : nil
        now_continuing = nil
      }
      return body
    end
  end
end


#
# 以下はこのファイルを実行プログラムとして動かした際の動作.
#
if $0 == __FILE__
  opt = OptionParser.new
  OPTS = {}
  ARGV.options{|opt|
    opt.on( '-X=VAL', '--exclude=VAL',
            "Exclude files (delimiter is \",\")"
            ){|v| OPTS[:exclude] = v.gsub(/^=/, '')}

    opt.on_tail('-q', '--quiet',
                "Progress message is not displayed"
                ){|v| OPTS[:quiet] = v}

    opt.on_tail('-v', '--verbose',
                "Verbose message is displayed"
                ){|v| OPTS[:verbose] = v}

    opt.on_tail('-S', '--strict',
                "Fortran 90/95 files are strictly parsed.
                                     (But take a long time.)"
                ){|v| OPTS[:strict] = v}

    opt.on_tail('-h', '-H', '--help',
                "This help message is output"
                ){|v| OPTS[:help] = v}
  opt.parse!
  }

  if OPTS[:help] || ARGV.size < 1
    print <<-"EOF"

  #{File.basename($0.to_s)}:

    USAGE: #{File.basename($0.to_s)} f90files [-X=exclude_file,...] [options]

    OPTION: \n#{opt.to_a[1..-1].join("")}
    EOF
    exit
  end

  f90dep = Fortran90Dependency.new(OPTS[:quiet], OPTS[:verbose], OPTS[:strict])
  f90dep.add_files(ARGV)
  f90dep.del_files(OPTS[:exclude].split(',')) if OPTS[:exclude]
  print f90dep.gen_list

end

#!/usr/bin/env ruby
# -*- coding: utf-8 -*-

class ColorizedString < String
  def colorize(color_code)
    "\e[#{color_code}m#{self}\e[0m" 
  end
  def red ; colorize(31) ; end
  def green ; colorize(32) ; end
  def yellow ; colorize(33) ; end
  def pink ; colorize(35) ; end
end

checks = %w{
  console¥.log
  Dumper
  DB::single
}

diff_cmd = "git diff --cached" 

errors = `#{diff_cmd} --name-only HEAD`.each_line.inject([]) do |errors, filename|
  filename.chomp!

  changes = `#{diff_cmd} --cached -U0 HEAD -- #{filename}`.split(/\n/)
puts changes
  checks.inject(errors) do |acc, check|
    unless (result = changes.grep(/^\+.*\b#{check}\b/)).empty?
      acc << {:name => check, :file =>  filename, :matches => result}
    else
      acc
    end
  end
end

unless errors.empty?
  errors.each do |error|
    puts ColorizedString.new("'#{error[:name]}' found in file: #{error[:file]}").yellow
    error[:matches].each {|m| puts "  -> #{m}" }
  end
  puts ColorizedString.new("COMMIT REJECTED. Please remove them before commiting OR use --no-verify").red
  exit 1
end

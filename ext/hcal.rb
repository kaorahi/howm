#!/usr/bin/ruby -sKe
# -*- Ruby -*-

# require 'jcode'
require 'iconv'
require 'date'

def usage
  name = File::basename $0
  print <<EOU
#{name}: 予定や todo の一覧を出力
(例)
  #{name} *.memo  → カレンダー(予定・〆切・済みの一覧)を出力
  #{name} -l *.memo  → 旬度順の todo 一覧を出力
  #{name} -i *.memo  → iCalendar 形式で出力 (作成中)
  #{name} -h または #{name} -help  → このメッセージを出力
(項目の書式)
  http://howm.sourceforge.jp/ 参照
EOU
end

############################################
# const

$schedule_types ||= '[@!.]'
$type_alias = {'' => '-'}
$date_sep = '-'

def item_regexp(types)
  x = $date_sep
  if $format == 'old'
    $todo_types ||= '[-+~!.]?'
    %r|(@\[(\d\d\d\d)#{x}(\d\d)#{x}(\d\d)\](#{types})(\d*)\s+(.*))$|
  else
    $todo_types ||= '[-+~!.]'
    %r|(\[(\d\d\d\d)#{x}(\d\d)#{x}(\d\d)\](#{types})(\d*)\s+(.*))$|
  end
end

# calendar

$comment_width ||= 12
$comment_width = $comment_width.to_i

$schedule_mark ||= '@'
$deadline_mark ||= '!'
$done_mark ||= '.'
$type_display = {
  '@' => $schedule_mark,
  '!' => $deadline_mark,
  '.' => $done_mark,
  '?' => '▲'
}
$type_order = {'@' => 2, '!' => 1, '.' => 3, '?' => 4}
$today_mark ||= ' <<<<##>>>>'
$same_day_mark ||= ' #>>>>'

# todo

$priority_func = {
#  ''  => lambda{|lt, lz| pr_normal lt, lz},
  '-' => lambda{|lt, lz| pr_normal lt, lz},
  '+' => lambda{|lt, lz| pr_todo lt, lz},
  '~' => lambda{|lt, lz| pr_defer lt, lz},
  '!' => lambda{|lt, lz| pr_deadline lt, lz},
  '@' => lambda{|lt, lz| pr_schedule lt, lz},
  '.' => lambda{|lt, lz| pr_done lt, lz},
  '?' => lambda{|lt, lz| 0},
}

# defaults
$lz_normal = 1
$lz_todo = 7
$lz_defer = 30
$lz_deadline = 7

# init
$pr_todo = -7
$pr_defer = -14
$pr_defer_peak = 0
$pr_deadline = -2

$huge = 77777
$huger = 88888
$pr_normal_bottom   = - $huge 
$pr_todo_bottom     = - $huge 
$pr_defer_bottom    = - $huge 
$pr_deadline_bottom = - $huge 
$pr_deadline_top    = $huge        
$pr_done_bottom     = - $huger

# misc

$now = Time::now.to_f
$daysec = 60 * 60 * 24

############################################
# func

def late(time)
  ($now - time.to_f) / $daysec
end

# def late(y, m, d, now)
#   ($now - Time::local(y,m,d,0,0,0).to_f) / $daysec
# end

def relative_late(late, laziness, default)
  laziness = default if laziness == 0
  late / laziness
end

def pr_normal(lt, lz)
  r = relative_late lt, lz, $lz_normal
  r >= 0 ? - r : $pr_normal_bottom + r
end

def pr_todo(lt, lz)
  r = relative_late lt, lz, $lz_todo
  c = - $pr_todo
  r >= 0 ? c * (r - 1) : $pr_todo_bottom + r
end

def pr_defer(lt, lz)
  r = relative_late lt, lz, $lz_defer
  c = $pr_defer_peak - $pr_defer
  v = 2 * (((r % 1) - 0.5).abs)
  r >= 0 ? $pr_defer_peak - c * v : $pr_defer_bottom + r
end

def pr_deadline(lt, lz)
  r = relative_late lt, lz, $lz_deadline
  c = - $pr_deadline
  if r > 0
    $pr_deadline_top + r
  elsif r < -1
    $pr_deadline_bottom + r
  else
    c * r
  end
end

# dummy
def pr_schedule(lt, lz)
  0
end

def pr_done(lt, lz)
  $pr_done_bottom + lt
end

############################################
# main

if ($help || $h)
  usage
  exit 0
end

def item(types)
  ARGF.grep(item_regexp(types)){|x|
    h = Hash::new
    h[:text] = $1
    y, m, d = [$2, $3, $4].map{|s| s.to_i}
#     h[:y] = y = $2.to_i
#     h[:m] = m = $3.to_i
#     h[:d] = d = $4.to_i
    h[:time] = time = Time::mktime(y, m, d)
    h[:type] = type = $type_alias[$5] || $5
    h[:laziness] = laziness = $6.to_i
    h[:comment] = $7
    h[:priority] = - $priority_func[type].call(late(time), laziness)
    h[:file] = ARGF.filename
    h[:line] = ARGF.file.lineno
    h
  }
end

# def select_type(item, types)
#   item.select{|h| types.member? h[:type]}
# end

### todo

def todo()
  item($todo_types).sort{
    |a, b| a[:priority] <=> b[:priority]
  }.each{|a|
    puts "#{a[:file]}:#{a[:line]}:#{a[:text]}"
  }
end

### ical

$conv = Iconv.new("UTF-8", "EUC-JP")

def ical()
  puts <<_EOS_
BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//howm.sourceforge.jp//hcal.rb $Revision: 1.12 $//EN
CALSCALE:Gregorian
#{item($schedule_types).map{|h| ical_item h}.select{|z| z}.join.chomp}
END:VCALENDAR
_EOS_
end

def ical_item(h)
#    if !Date.valid_date?(h[:y], h[:m], 1)
#      $stderr.puts 'Invalid date:#{h[:file]}:#{h[:line]}:#{h[:text]}'
#      return nil
#    end
#   # convert 2005-09-31 to 2005-10-01
#   d = Date.new(h[:y], h[:m], 1) + (h[:d] - 1)
#   d = Date.new h[:y], h[:m], h[:d]
#   s, e = [d, d+1].map{|z| z.strftime '%Y%m%d'}
  s, e = [h[:time], h[:time] + 86400].map{|z| z.strftime '%Y%m%d'}
  return <<_EOS_
BEGIN:VEVENT
DTSTART:#{s}
DTEND:#{e}
SUMMARY:#{$conv.iconv(h[:type] + h[:comment])}
END:VEVENT
_EOS_
end

### schedule

def schedule()
  cal = Hash::new
  item($schedule_types).each{|h|
    t = h[:time]
    cal[t] ||= Array::new
    cal[t].push h
    # [2004-12-25]@3 ==> [2004-12-25]@ [2004-12-26]@ [2004-12-27]@
    if h[:type] == '@' && h[:laziness] > 1
      (1...h[:laziness]).each{|d|
        td = t + 60 * 60 * 24 * d
        cal[td] ||= Array::new
        cal[td].push h
      }
    end
  }
  min_time = cal.keys.min
  max_time = cal.keys.max
  t = min_time
  while t <= max_time
    c = cal[t] || []
    puts if t.wday == 0
    puts "----------------<#{t.month}>---------------- #{t.year}" if t.day == 1
    day = t.strftime '%d %a'
    text = c.sort{|a,b|
      x, y = [a, b].map{|z| [$type_order[z[:type]], z[:comment]]}
      x <=> y
#       $type_order[a[:type]] <=> $type_order[b[:type]]
    }.map{|h|
      h[:comment].sub!(%r|^(cancel)? *\[[#{$date_sep}0-9]+\][!+]?[0-9]*\s*|){|s| $1 ? 'x' : ''} if h[:type] == '.'  # adhoc!
      $type_display[h[:type]] + h[:comment].split(//)[0, $comment_width].join
    }.join ' '
    mark = if t.strftime('%Y%m%d') == Time::now.strftime('%Y%m%d')
             $today_mark
           elsif t.strftime('%m%d') == Time::now.strftime('%m%d')
             $same_day_mark
           else
             ''
           end
    puts "#{day} #{text}#{mark}"
    t += 60*60*24
  end
end

### main

if $l
  todo
elsif $i
  ical
else
  schedule
end

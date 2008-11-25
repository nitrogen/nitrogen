#!/usr/bin/ruby

require 'YAML'

class App
  def initialize(arguments, stdin)
    @arguments = arguments
  end
  
  def run
    @arguments.each do |arg|
      print "Checking #{arg}..."

      # Load the file...
      yaml_content = File.read(arg)
      node = YAML::load(yaml_content)
      
      # Create the html...
      title = node['title']
      see_also = see_also_to_html(node['see_also'])
      description = node['description']
      attributes = attributes_to_html(node['attributes'])
      html = page_to_html(title, see_also, description, attributes)
      
      output_file_name = File::basename(arg, ".yml")
      output_file = File.new("../doc/#{output_file_name}.html", "w")
      output_file.print html
      output_file.close
      
      print "ok.\r\n"
    end
  end
  
  
  def page_to_html(title, see_also, description, attributes)
    "
      <html>
      <head>
        <title>Nitrogen Docs - #{title}</title>
        <link href='style.css' type='text/css' rel='stylesheet'>
      </head>
      <body>
      <div class=content>
        <div class=title>#{title}</div>
        <div class=description_label>Description</div>
        <div class=description>#{description}</div>
        <div class=attributes_label>Attributes</div>
        <div class=attributes>#{attributes}</div>
        <div class=see_also_label>See Also</div>
        <div class=see_also>#{see_also}</div>
      </div>
      </html>
    "
  end
  
  def see_also_to_html(see_also)
    if (see_also == nil) then return "" end
      
    return_val = ""
    see_also.split(", ").each do |ref| 
      return_val += "<a href=element_#{ref}.html>#{ref}</a>&nbsp;\r\n"
    end
    return return_val
  end
  
  def attributes_to_html(attributes)
    if (attributes == nil) then return "" end

    attributes.map { |attribute|
      attribute_to_html(attribute)
    }
  end
  
  def attribute_to_html(attribute)
    return " 
    <div class=attribute>
      <div class=name>#{attribute['name']}</div>
      <div class=type>#{attribute['type']}</div>
      <div class=description>#{attribute['description']}</div>
    </div>
    "
  end
end

app = App.new(ARGV, STDIN)
app.run


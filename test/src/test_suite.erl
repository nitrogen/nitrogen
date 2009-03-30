-module(test_suite).

-author("michael@mullistechnologies.com").

-include_lib("eunit/include/eunit.hrl").

all_test_() ->
  [{module, element_br_test},
   {module, element_hr_test},
   {module, element_image_test},
   {module, element_label_test},
   {module, element_link_test},
   {module, element_list_test},
   {module, element_listitem_test},
   {module, element_literal_test},
   {module, element_p_test},
   {module, element_span_test},
   {module, element_value_test},

	 {module, element_textarea_test},

   {module, action_add_class_test},
   {module, action_alert_test},
   {module, action_jquery_effect_test},
   {module, action_animate_test}
   
  ].

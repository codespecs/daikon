<instlist>

<instance name="state">
<domain name="Color">
<atom>Red</atom>
<atom>Black</atom>
</domain>
<domain name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</domain>
<set name="Color">
<atom>Red</atom>
<atom>Black</atom>
</set>
<set name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="Black">
<atom>Black</atom>
</set>
<set name="NIL">
<atom>NIL</atom>
</set>
<set name="One">
<atom>One</atom>
</set>
<set name="Red">
<atom>Red</atom>
</set>
<set name="Three">
<atom>Three</atom>
</set>
<set name="Two">
<atom>Two</atom>
</set>
<set name="Zero">
<atom>Zero</atom>
</set>
<set name="nodes_Input">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="remove">
<atom>Two</atom>
</set>
<set name="root_Input">
<atom>One</atom>
</set>
<relation name="child_Input">
<map>
<atom>Two</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Three</atom>
</range>
</map>
</relation>
<relation name="color_Input">
<map>
<atom>Three</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Red</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Red</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Black</atom>
</range>
</map>
</relation>
<relation name="left_Input">
<map>
<atom>Two</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
</relation>
<relation name="parent_Input">
<map>
<atom>NIL</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Zero</atom>
</range>
</map>
</relation>
<relation name="right_Input">
<map>
<atom>One</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
</instance>



<instance name="state">
<domain name="Color">
<atom>Red</atom>
<atom>Black</atom>
</domain>
<domain name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</domain>
<set name="Color">
<atom>Red</atom>
<atom>Black</atom>
</set>
<set name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="Black">
<atom>Black</atom>
</set>
<set name="NIL">
<atom>NIL</atom>
</set>
<set name="One">
<atom>One</atom>
</set>
<set name="Red">
<atom>Red</atom>
</set>
<set name="Three">
<atom>Three</atom>
</set>
<set name="Two">
<atom>Two</atom>
</set>
<set name="Zero">
<atom>Zero</atom>
</set>
<set name="nodes_Input">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="remove">
<atom>Zero</atom>
</set>
<set name="root_Input">
<atom>Two</atom>
</set>
<relation name="child_Input">
<map>
<atom>Two</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
<relation name="color_Input">
<map>
<atom>Zero</atom>
<range>
<atom>Red</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Black</atom>
</range>
</map>
</relation>
<relation name="left_Input">
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
<relation name="parent_Input">
<map>
<atom>NIL</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>One</atom>
</range>
</map>
</relation>
<relation name="right_Input">
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
</instance>



<instance name="state">
<domain name="Color">
<atom>Red</atom>
<atom>Black</atom>
</domain>
<domain name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</domain>
<set name="Color">
<atom>Red</atom>
<atom>Black</atom>
</set>
<set name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="Black">
<atom>Black</atom>
</set>
<set name="NIL">
<atom>NIL</atom>
</set>
<set name="One">
<atom>One</atom>
</set>
<set name="Red">
<atom>Red</atom>
</set>
<set name="Three">
<atom>Three</atom>
</set>
<set name="Two">
<atom>Two</atom>
</set>
<set name="Zero">
<atom>Zero</atom>
</set>
<set name="nodes_Input">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="remove">
<atom>One</atom>
</set>
<set name="root_Input">
<atom>Two</atom>
</set>
<relation name="child_Input">
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
<relation name="color_Input">
<map>
<atom>Three</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>Red</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Black</atom>
</range>
</map>
</relation>
<relation name="left_Input">
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>One</atom>
</range>
</map>
</relation>
<relation name="parent_Input">
<map>
<atom>NIL</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>One</atom>
</range>
</map>
</relation>
<relation name="right_Input">
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Three</atom>
</range>
</map>
</relation>
</instance>



<instance name="state">
<domain name="Color">
<atom>Red</atom>
<atom>Black</atom>
</domain>
<domain name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</domain>
<set name="Color">
<atom>Red</atom>
<atom>Black</atom>
</set>
<set name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="Black">
<atom>Black</atom>
</set>
<set name="NIL">
<atom>NIL</atom>
</set>
<set name="One">
<atom>One</atom>
</set>
<set name="Red">
<atom>Red</atom>
</set>
<set name="Three">
<atom>Three</atom>
</set>
<set name="Two">
<atom>Two</atom>
</set>
<set name="Zero">
<atom>Zero</atom>
</set>
<set name="nodes_Input">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="remove">
<atom>Three</atom>
</set>
<set name="root_Input">
<atom>Two</atom>
</set>
<relation name="child_Input">
<map>
<atom>Two</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
<relation name="color_Input">
<map>
<atom>One</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>Red</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Black</atom>
</range>
</map>
</relation>
<relation name="left_Input">
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>One</atom>
</range>
</map>
</relation>
<relation name="parent_Input">
<map>
<atom>NIL</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Three</atom>
</range>
</map>
</relation>
<relation name="right_Input">
<map>
<atom>One</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Three</atom>
</range>
</map>
</relation>
</instance>



<instance name="state">
<domain name="Color">
<atom>Red</atom>
<atom>Black</atom>
</domain>
<domain name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</domain>
<set name="Color">
<atom>Red</atom>
<atom>Black</atom>
</set>
<set name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="Black">
<atom>Black</atom>
</set>
<set name="NIL">
<atom>NIL</atom>
</set>
<set name="One">
<atom>One</atom>
</set>
<set name="Red">
<atom>Red</atom>
</set>
<set name="Three">
<atom>Three</atom>
</set>
<set name="Two">
<atom>Two</atom>
</set>
<set name="Zero">
<atom>Zero</atom>
</set>
<set name="nodes_Input">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="remove">
<atom>Two</atom>
</set>
<set name="root_Input">
<atom>Two</atom>
</set>
<relation name="child_Input">
<map>
<atom>One</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>One</atom>
</range>
</map>
</relation>
<relation name="color_Input">
<map>
<atom>Two</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>Red</atom>
</range>
</map>
</relation>
<relation name="left_Input">
<map>
<atom>Two</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
<relation name="parent_Input">
<map>
<atom>NIL</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>One</atom>
</range>
</map>
</relation>
<relation name="right_Input">
<map>
<atom>Two</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
</instance>



<instance name="state">
<domain name="Color">
<atom>Red</atom>
<atom>Black</atom>
</domain>
<domain name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</domain>
<set name="Color">
<atom>Red</atom>
<atom>Black</atom>
</set>
<set name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="Black">
<atom>Black</atom>
</set>
<set name="NIL">
<atom>NIL</atom>
</set>
<set name="One">
<atom>One</atom>
</set>
<set name="Red">
<atom>Red</atom>
</set>
<set name="Three">
<atom>Three</atom>
</set>
<set name="Two">
<atom>Two</atom>
</set>
<set name="Zero">
<atom>Zero</atom>
</set>
<set name="nodes_Input">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="remove">
<atom>Zero</atom>
</set>
<set name="root_Input">
<atom>Two</atom>
</set>
<relation name="child_Input">
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
<relation name="color_Input">
<map>
<atom>Three</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Red</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Black</atom>
</range>
</map>
</relation>
<relation name="left_Input">
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Zero</atom>
</range>
</map>
</relation>
<relation name="parent_Input">
<map>
<atom>Zero</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Three</atom>
</range>
</map>
</relation>
<relation name="right_Input">
<map>
<atom>One</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
</instance>



<instance name="state">
<domain name="Color">
<atom>Red</atom>
<atom>Black</atom>
</domain>
<domain name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</domain>
<set name="Color">
<atom>Red</atom>
<atom>Black</atom>
</set>
<set name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="Black">
<atom>Black</atom>
</set>
<set name="NIL">
<atom>NIL</atom>
</set>
<set name="One">
<atom>One</atom>
</set>
<set name="Red">
<atom>Red</atom>
</set>
<set name="Three">
<atom>Three</atom>
</set>
<set name="Two">
<atom>Two</atom>
</set>
<set name="Zero">
<atom>Zero</atom>
</set>
<set name="nodes_Input">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="remove">
<atom>One</atom>
</set>
<set name="root_Input">
<atom>Two</atom>
</set>
<relation name="child_Input">
<map>
<atom>Two</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Three</atom>
</range>
</map>
</relation>
<relation name="color_Input">
<map>
<atom>One</atom>
<range>
<atom>Red</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>Black</atom>
</range>
</map>
</relation>
<relation name="left_Input">
<map>
<atom>Two</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
<relation name="parent_Input">
<map>
<atom>NIL</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>Two</atom>
</range>
</map>
</relation>
<relation name="right_Input">
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>One</atom>
</range>
</map>
</relation>
</instance>



<instance name="state">
<domain name="Color">
<atom>Red</atom>
<atom>Black</atom>
</domain>
<domain name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</domain>
<set name="Color">
<atom>Red</atom>
<atom>Black</atom>
</set>
<set name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="Black">
<atom>Black</atom>
</set>
<set name="NIL">
<atom>NIL</atom>
</set>
<set name="One">
<atom>One</atom>
</set>
<set name="Red">
<atom>Red</atom>
</set>
<set name="Three">
<atom>Three</atom>
</set>
<set name="Two">
<atom>Two</atom>
</set>
<set name="Zero">
<atom>Zero</atom>
</set>
<set name="nodes_Input">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="remove">
<atom>Two</atom>
</set>
<set name="root_Input">
<atom>Two</atom>
</set>
<relation name="child_Input">
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
<relation name="color_Input">
<map>
<atom>Two</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Red</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>Black</atom>
</range>
</map>
</relation>
<relation name="left_Input">
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
<relation name="parent_Input">
<map>
<atom>NIL</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>One</atom>
</range>
</map>
</relation>
<relation name="right_Input">
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Three</atom>
</range>
</map>
</relation>
</instance>



<instance name="state">
<domain name="Color">
<atom>Red</atom>
<atom>Black</atom>
</domain>
<domain name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</domain>
<set name="Color">
<atom>Red</atom>
<atom>Black</atom>
</set>
<set name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="Black">
<atom>Black</atom>
</set>
<set name="NIL">
<atom>NIL</atom>
</set>
<set name="One">
<atom>One</atom>
</set>
<set name="Red">
<atom>Red</atom>
</set>
<set name="Three">
<atom>Three</atom>
</set>
<set name="Two">
<atom>Two</atom>
</set>
<set name="Zero">
<atom>Zero</atom>
</set>
<set name="nodes_Input">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="remove">
<atom>Three</atom>
</set>
<set name="root_Input">
<atom>Two</atom>
</set>
<relation name="child_Input">
<map>
<atom>Two</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
<relation name="color_Input">
<map>
<atom>Three</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Red</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Black</atom>
</range>
</map>
</relation>
<relation name="left_Input">
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Zero</atom>
</range>
</map>
</relation>
<relation name="parent_Input">
<map>
<atom>Three</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Three</atom>
</range>
</map>
</relation>
<relation name="right_Input">
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>One</atom>
</range>
</map>
</relation>
</instance>



<instance name="state">
<domain name="Color">
<atom>Red</atom>
<atom>Black</atom>
</domain>
<domain name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</domain>
<set name="Color">
<atom>Red</atom>
<atom>Black</atom>
</set>
<set name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="Black">
<atom>Black</atom>
</set>
<set name="NIL">
<atom>NIL</atom>
</set>
<set name="One">
<atom>One</atom>
</set>
<set name="Red">
<atom>Red</atom>
</set>
<set name="Three">
<atom>Three</atom>
</set>
<set name="Two">
<atom>Two</atom>
</set>
<set name="Zero">
<atom>Zero</atom>
</set>
<set name="nodes_Input">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="remove">
<atom>Zero</atom>
</set>
<set name="root_Input">
<atom>Two</atom>
</set>
<relation name="child_Input">
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
<relation name="color_Input">
<map>
<atom>Three</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Red</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Red</atom>
</range>
</map>
</relation>
<relation name="left_Input">
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Zero</atom>
</range>
</map>
</relation>
<relation name="parent_Input">
<map>
<atom>Three</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>Two</atom>
</range>
</map>
</relation>
<relation name="right_Input">
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>One</atom>
</range>
</map>
</relation>
</instance>



<instance name="state">
<domain name="Color">
<atom>Red</atom>
<atom>Black</atom>
</domain>
<domain name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</domain>
<set name="Color">
<atom>Red</atom>
<atom>Black</atom>
</set>
<set name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="Black">
<atom>Black</atom>
</set>
<set name="NIL">
<atom>NIL</atom>
</set>
<set name="One">
<atom>One</atom>
</set>
<set name="Red">
<atom>Red</atom>
</set>
<set name="Three">
<atom>Three</atom>
</set>
<set name="Two">
<atom>Two</atom>
</set>
<set name="Zero">
<atom>Zero</atom>
</set>
<set name="nodes_Input">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="remove">
<atom>One</atom>
</set>
<set name="root_Input">
<atom>Two</atom>
</set>
<relation name="child_Input">
<map>
<atom>One</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
<relation name="color_Input">
<map>
<atom>Two</atom>
<range>
<atom>Red</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Red</atom>
</range>
</map>
</relation>
<relation name="left_Input">
<map>
<atom>One</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
<relation name="parent_Input">
<map>
<atom>NIL</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Three</atom>
</range>
</map>
</relation>
<relation name="right_Input">
<map>
<atom>One</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>One</atom>
</range>
</map>
</relation>
</instance>



<instance name="state">
<domain name="Color">
<atom>Red</atom>
<atom>Black</atom>
</domain>
<domain name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</domain>
<set name="Color">
<atom>Red</atom>
<atom>Black</atom>
</set>
<set name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="Black">
<atom>Black</atom>
</set>
<set name="NIL">
<atom>NIL</atom>
</set>
<set name="One">
<atom>One</atom>
</set>
<set name="Red">
<atom>Red</atom>
</set>
<set name="Three">
<atom>Three</atom>
</set>
<set name="Two">
<atom>Two</atom>
</set>
<set name="Zero">
<atom>Zero</atom>
</set>
<set name="nodes_Input">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="remove">
<atom>Two</atom>
</set>
<set name="root_Input">
<atom>Two</atom>
</set>
<relation name="child_Input">
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
<relation name="color_Input">
<map>
<atom>NIL</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Red</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Red</atom>
</range>
</map>
</relation>
<relation name="left_Input">
<map>
<atom>Two</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
<relation name="parent_Input">
<map>
<atom>Three</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Zero</atom>
</range>
</map>
</relation>
<relation name="right_Input">
<map>
<atom>One</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Three</atom>
</range>
</map>
</relation>
</instance>



<instance name="state">
<domain name="Color">
<atom>Red</atom>
<atom>Black</atom>
</domain>
<domain name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</domain>
<set name="Color">
<atom>Red</atom>
<atom>Black</atom>
</set>
<set name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="Black">
<atom>Black</atom>
</set>
<set name="NIL">
<atom>NIL</atom>
</set>
<set name="One">
<atom>One</atom>
</set>
<set name="Red">
<atom>Red</atom>
</set>
<set name="Three">
<atom>Three</atom>
</set>
<set name="Two">
<atom>Two</atom>
</set>
<set name="Zero">
<atom>Zero</atom>
</set>
<set name="nodes_Input">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="remove">
<atom>Three</atom>
</set>
<set name="root_Input">
<atom>Two</atom>
</set>
<relation name="child_Input">
<map>
<atom>Two</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
<relation name="color_Input">
<map>
<atom>One</atom>
<range>
<atom>Red</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Red</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Black</atom>
</range>
</map>
</relation>
<relation name="left_Input">
<map>
<atom>Two</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
<relation name="parent_Input">
<map>
<atom>NIL</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Zero</atom>
</range>
</map>
</relation>
<relation name="right_Input">
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Three</atom>
</range>
</map>
</relation>
</instance>



<instance name="state">
<domain name="Color">
<atom>Red</atom>
<atom>Black</atom>
</domain>
<domain name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</domain>
<set name="Color">
<atom>Red</atom>
<atom>Black</atom>
</set>
<set name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="Black">
<atom>Black</atom>
</set>
<set name="NIL">
<atom>NIL</atom>
</set>
<set name="One">
<atom>One</atom>
</set>
<set name="Red">
<atom>Red</atom>
</set>
<set name="Three">
<atom>Three</atom>
</set>
<set name="Two">
<atom>Two</atom>
</set>
<set name="Zero">
<atom>Zero</atom>
</set>
<set name="nodes_Input">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="remove">
<atom>Zero</atom>
</set>
<set name="root_Input">
<atom>Two</atom>
</set>
<relation name="child_Input">
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
<relation name="color_Input">
<map>
<atom>Two</atom>
<range>
<atom>Red</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>Red</atom>
</range>
</map>
</relation>
<relation name="left_Input">
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
</relation>
<relation name="parent_Input">
<map>
<atom>NIL</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>Two</atom>
</range>
</map>
</relation>
<relation name="right_Input">
<map>
<atom>One</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Three</atom>
</range>
</map>
</relation>
</instance>



<instance name="state">
<domain name="Color">
<atom>Red</atom>
<atom>Black</atom>
</domain>
<domain name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</domain>
<set name="Color">
<atom>Red</atom>
<atom>Black</atom>
</set>
<set name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="Black">
<atom>Black</atom>
</set>
<set name="NIL">
<atom>NIL</atom>
</set>
<set name="One">
<atom>One</atom>
</set>
<set name="Red">
<atom>Red</atom>
</set>
<set name="Three">
<atom>Three</atom>
</set>
<set name="Two">
<atom>Two</atom>
</set>
<set name="Zero">
<atom>Zero</atom>
</set>
<set name="nodes_Input">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="remove">
<atom>One</atom>
</set>
<set name="root_Input">
<atom>Two</atom>
</set>
<relation name="child_Input">
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
<relation name="color_Input">
<map>
<atom>Three</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Red</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>Red</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Black</atom>
</range>
</map>
</relation>
<relation name="left_Input">
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
<relation name="parent_Input">
<map>
<atom>NIL</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>One</atom>
</range>
</map>
</relation>
<relation name="right_Input">
<map>
<atom>Two</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
</instance>



<instance name="state">
<domain name="Color">
<atom>Red</atom>
<atom>Black</atom>
</domain>
<domain name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</domain>
<set name="Color">
<atom>Red</atom>
<atom>Black</atom>
</set>
<set name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="Black">
<atom>Black</atom>
</set>
<set name="NIL">
<atom>NIL</atom>
</set>
<set name="One">
<atom>One</atom>
</set>
<set name="Red">
<atom>Red</atom>
</set>
<set name="Three">
<atom>Three</atom>
</set>
<set name="Two">
<atom>Two</atom>
</set>
<set name="Zero">
<atom>Zero</atom>
</set>
<set name="nodes_Input">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="remove">
<atom>Two</atom>
</set>
<set name="root_Input">
<atom>Two</atom>
</set>
<relation name="child_Input">
<map>
<atom>Two</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>One</atom>
</range>
</map>
</relation>
<relation name="color_Input">
<map>
<atom>Zero</atom>
<range>
<atom>Red</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Red</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Black</atom>
</range>
</map>
</relation>
<relation name="left_Input">
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
</relation>
<relation name="parent_Input">
<map>
<atom>Zero</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Zero</atom>
</range>
</map>
</relation>
<relation name="right_Input">
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
</instance>



<instance name="state">
<domain name="Color">
<atom>Red</atom>
<atom>Black</atom>
</domain>
<domain name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</domain>
<set name="Color">
<atom>Red</atom>
<atom>Black</atom>
</set>
<set name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="Black">
<atom>Black</atom>
</set>
<set name="NIL">
<atom>NIL</atom>
</set>
<set name="One">
<atom>One</atom>
</set>
<set name="Red">
<atom>Red</atom>
</set>
<set name="Three">
<atom>Three</atom>
</set>
<set name="Two">
<atom>Two</atom>
</set>
<set name="Zero">
<atom>Zero</atom>
</set>
<set name="nodes_Input">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="remove">
<atom>Three</atom>
</set>
<set name="root_Input">
<atom>Two</atom>
</set>
<relation name="child_Input">
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
<relation name="color_Input">
<map>
<atom>One</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>Red</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Red</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Black</atom>
</range>
</map>
</relation>
<relation name="left_Input">
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>One</atom>
</range>
</map>
</relation>
<relation name="parent_Input">
<map>
<atom>Zero</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>One</atom>
</range>
</map>
</relation>
<relation name="right_Input">
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
</instance>



<instance name="state">
<domain name="Color">
<atom>Red</atom>
<atom>Black</atom>
</domain>
<domain name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</domain>
<set name="Color">
<atom>Red</atom>
<atom>Black</atom>
</set>
<set name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="Black">
<atom>Black</atom>
</set>
<set name="NIL">
<atom>NIL</atom>
</set>
<set name="One">
<atom>One</atom>
</set>
<set name="Red">
<atom>Red</atom>
</set>
<set name="Three">
<atom>Three</atom>
</set>
<set name="Two">
<atom>Two</atom>
</set>
<set name="Zero">
<atom>Zero</atom>
</set>
<set name="nodes_Input">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="remove">
<atom>Zero</atom>
</set>
<set name="root_Input">
<atom>One</atom>
</set>
<relation name="child_Input">
<map>
<atom>One</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
<relation name="color_Input">
<map>
<atom>Two</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>Red</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Red</atom>
</range>
</map>
</relation>
<relation name="left_Input">
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
<relation name="parent_Input">
<map>
<atom>NIL</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>One</atom>
</range>
</map>
</relation>
<relation name="right_Input">
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Two</atom>
</range>
</map>
</relation>
</instance>



<instance name="state">
<domain name="Color">
<atom>Red</atom>
<atom>Black</atom>
</domain>
<domain name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</domain>
<set name="Color">
<atom>Red</atom>
<atom>Black</atom>
</set>
<set name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="Black">
<atom>Black</atom>
</set>
<set name="NIL">
<atom>NIL</atom>
</set>
<set name="One">
<atom>One</atom>
</set>
<set name="Red">
<atom>Red</atom>
</set>
<set name="Three">
<atom>Three</atom>
</set>
<set name="Two">
<atom>Two</atom>
</set>
<set name="Zero">
<atom>Zero</atom>
</set>
<set name="nodes_Input">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="remove">
<atom>One</atom>
</set>
<set name="root_Input">
<atom>One</atom>
</set>
<relation name="child_Input">
<map>
<atom>Two</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Two</atom>
</range>
</map>
</relation>
<relation name="color_Input">
<map>
<atom>Zero</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>Red</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Red</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Black</atom>
</range>
</map>
</relation>
<relation name="left_Input">
<map>
<atom>Two</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
<relation name="parent_Input">
<map>
<atom>NIL</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Zero</atom>
</range>
</map>
</relation>
<relation name="right_Input">
<map>
<atom>Two</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
</instance>



<instance name="state">
<domain name="Color">
<atom>Red</atom>
<atom>Black</atom>
</domain>
<domain name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</domain>
<set name="Color">
<atom>Red</atom>
<atom>Black</atom>
</set>
<set name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="Black">
<atom>Black</atom>
</set>
<set name="NIL">
<atom>NIL</atom>
</set>
<set name="One">
<atom>One</atom>
</set>
<set name="Red">
<atom>Red</atom>
</set>
<set name="Three">
<atom>Three</atom>
</set>
<set name="Two">
<atom>Two</atom>
</set>
<set name="Zero">
<atom>Zero</atom>
</set>
<set name="nodes_Input">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="remove">
<atom>Three</atom>
</set>
<set name="root_Input">
<atom>One</atom>
</set>
<relation name="child_Input">
<map>
<atom>One</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
<relation name="color_Input">
<map>
<atom>NIL</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Red</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>Red</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>Black</atom>
</range>
</map>
</relation>
<relation name="left_Input">
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
<relation name="parent_Input">
<map>
<atom>NIL</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Zero</atom>
</range>
</map>
</relation>
<relation name="right_Input">
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Three</atom>
</range>
</map>
</relation>
</instance>



<instance name="state">
<domain name="Color">
<atom>Red</atom>
<atom>Black</atom>
</domain>
<domain name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</domain>
<set name="Color">
<atom>Red</atom>
<atom>Black</atom>
</set>
<set name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="Black">
<atom>Black</atom>
</set>
<set name="NIL">
<atom>NIL</atom>
</set>
<set name="One">
<atom>One</atom>
</set>
<set name="Red">
<atom>Red</atom>
</set>
<set name="Three">
<atom>Three</atom>
</set>
<set name="Two">
<atom>Two</atom>
</set>
<set name="Zero">
<atom>Zero</atom>
</set>
<set name="nodes_Input">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="remove">
<atom>Two</atom>
</set>
<set name="root_Input">
<atom>One</atom>
</set>
<relation name="child_Input">
<map>
<atom>Two</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
<relation name="color_Input">
<map>
<atom>Two</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Red</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>Red</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>Black</atom>
</range>
</map>
</relation>
<relation name="left_Input">
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
<relation name="parent_Input">
<map>
<atom>Three</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>One</atom>
</range>
</map>
</relation>
<relation name="right_Input">
<map>
<atom>One</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
</instance>



<instance name="state">
<domain name="Color">
<atom>Red</atom>
<atom>Black</atom>
</domain>
<domain name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</domain>
<set name="Color">
<atom>Red</atom>
<atom>Black</atom>
</set>
<set name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="Black">
<atom>Black</atom>
</set>
<set name="NIL">
<atom>NIL</atom>
</set>
<set name="One">
<atom>One</atom>
</set>
<set name="Red">
<atom>Red</atom>
</set>
<set name="Three">
<atom>Three</atom>
</set>
<set name="Two">
<atom>Two</atom>
</set>
<set name="Zero">
<atom>Zero</atom>
</set>
<set name="nodes_Input">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="remove">
<atom>One</atom>
</set>
<set name="root_Input">
<atom>One</atom>
</set>
<relation name="child_Input">
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
<relation name="color_Input">
<map>
<atom>Two</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>Red</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Black</atom>
</range>
</map>
</relation>
<relation name="left_Input">
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
<relation name="parent_Input">
<map>
<atom>NIL</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Zero</atom>
</range>
</map>
</relation>
<relation name="right_Input">
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Three</atom>
</range>
</map>
</relation>
</instance>



<instance name="state">
<domain name="Color">
<atom>Red</atom>
<atom>Black</atom>
</domain>
<domain name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</domain>
<set name="Color">
<atom>Red</atom>
<atom>Black</atom>
</set>
<set name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="Black">
<atom>Black</atom>
</set>
<set name="NIL">
<atom>NIL</atom>
</set>
<set name="One">
<atom>One</atom>
</set>
<set name="Red">
<atom>Red</atom>
</set>
<set name="Three">
<atom>Three</atom>
</set>
<set name="Two">
<atom>Two</atom>
</set>
<set name="Zero">
<atom>Zero</atom>
</set>
<set name="nodes_Input">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="remove">
<atom>Two</atom>
</set>
<set name="root_Input">
<atom>One</atom>
</set>
<relation name="child_Input">
<map>
<atom>Two</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
<relation name="color_Input">
<map>
<atom>One</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>Red</atom>
</range>
</map>
</relation>
<relation name="left_Input">
<map>
<atom>Two</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
<relation name="parent_Input">
<map>
<atom>Zero</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Three</atom>
</range>
</map>
</relation>
<relation name="right_Input">
<map>
<atom>One</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
</instance>



<instance name="state">
<domain name="Color">
<atom>Red</atom>
<atom>Black</atom>
</domain>
<domain name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</domain>
<set name="Color">
<atom>Red</atom>
<atom>Black</atom>
</set>
<set name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="Black">
<atom>Black</atom>
</set>
<set name="NIL">
<atom>NIL</atom>
</set>
<set name="One">
<atom>One</atom>
</set>
<set name="Red">
<atom>Red</atom>
</set>
<set name="Three">
<atom>Three</atom>
</set>
<set name="Two">
<atom>Two</atom>
</set>
<set name="Zero">
<atom>Zero</atom>
</set>
<set name="nodes_Input">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="remove">
<atom>Three</atom>
</set>
<set name="root_Input">
<atom>One</atom>
</set>
<relation name="child_Input">
<map>
<atom>Two</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
<relation name="color_Input">
<map>
<atom>Zero</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>Red</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Black</atom>
</range>
</map>
</relation>
<relation name="left_Input">
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
</relation>
<relation name="parent_Input">
<map>
<atom>Three</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Zero</atom>
</range>
</map>
</relation>
<relation name="right_Input">
<map>
<atom>One</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
</instance>



<instance name="state">
<domain name="Color">
<atom>Red</atom>
<atom>Black</atom>
</domain>
<domain name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</domain>
<set name="Color">
<atom>Red</atom>
<atom>Black</atom>
</set>
<set name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="Black">
<atom>Black</atom>
</set>
<set name="NIL">
<atom>NIL</atom>
</set>
<set name="One">
<atom>One</atom>
</set>
<set name="Red">
<atom>Red</atom>
</set>
<set name="Three">
<atom>Three</atom>
</set>
<set name="Two">
<atom>Two</atom>
</set>
<set name="Zero">
<atom>Zero</atom>
</set>
<set name="nodes_Input">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="remove">
<atom>Zero</atom>
</set>
<set name="root_Input">
<atom>One</atom>
</set>
<relation name="child_Input">
<map>
<atom>Two</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
<relation name="color_Input">
<map>
<atom>Two</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>Red</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Black</atom>
</range>
</map>
</relation>
<relation name="left_Input">
<map>
<atom>Two</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
<relation name="parent_Input">
<map>
<atom>NIL</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>One</atom>
</range>
</map>
</relation>
<relation name="right_Input">
<map>
<atom>Two</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
</instance>



<instance name="state">
<domain name="Color">
<atom>Red</atom>
<atom>Black</atom>
</domain>
<domain name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</domain>
<set name="Color">
<atom>Red</atom>
<atom>Black</atom>
</set>
<set name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="Black">
<atom>Black</atom>
</set>
<set name="NIL">
<atom>NIL</atom>
</set>
<set name="One">
<atom>One</atom>
</set>
<set name="Red">
<atom>Red</atom>
</set>
<set name="Three">
<atom>Three</atom>
</set>
<set name="Two">
<atom>Two</atom>
</set>
<set name="Zero">
<atom>Zero</atom>
</set>
<set name="nodes_Input">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="remove">
<atom>Zero</atom>
</set>
<set name="root_Input">
<atom>One</atom>
</set>
<relation name="child_Input">
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Three</atom>
</range>
</map>
</relation>
<relation name="color_Input">
<map>
<atom>NIL</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Red</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Red</atom>
</range>
</map>
</relation>
<relation name="left_Input">
<map>
<atom>Two</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
<relation name="parent_Input">
<map>
<atom>NIL</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Zero</atom>
</range>
</map>
</relation>
<relation name="right_Input">
<map>
<atom>One</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
</instance>



<instance name="state">
<domain name="Color">
<atom>Red</atom>
<atom>Black</atom>
</domain>
<domain name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</domain>
<set name="Color">
<atom>Red</atom>
<atom>Black</atom>
</set>
<set name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="Black">
<atom>Black</atom>
</set>
<set name="NIL">
<atom>NIL</atom>
</set>
<set name="One">
<atom>One</atom>
</set>
<set name="Red">
<atom>Red</atom>
</set>
<set name="Three">
<atom>Three</atom>
</set>
<set name="Two">
<atom>Two</atom>
</set>
<set name="Zero">
<atom>Zero</atom>
</set>
<set name="nodes_Input">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="remove">
<atom>One</atom>
</set>
<set name="root_Input">
<atom>One</atom>
</set>
<relation name="child_Input">
<map>
<atom>One</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
<relation name="color_Input">
<map>
<atom>NIL</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Red</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Red</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>Black</atom>
</range>
</map>
</relation>
<relation name="left_Input">
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
<relation name="parent_Input">
<map>
<atom>Three</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>One</atom>
</range>
</map>
</relation>
<relation name="right_Input">
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
</instance>



<instance name="state">
<domain name="Color">
<atom>Red</atom>
<atom>Black</atom>
</domain>
<domain name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</domain>
<set name="Color">
<atom>Red</atom>
<atom>Black</atom>
</set>
<set name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="Black">
<atom>Black</atom>
</set>
<set name="NIL">
<atom>NIL</atom>
</set>
<set name="One">
<atom>One</atom>
</set>
<set name="Red">
<atom>Red</atom>
</set>
<set name="Three">
<atom>Three</atom>
</set>
<set name="Two">
<atom>Two</atom>
</set>
<set name="Zero">
<atom>Zero</atom>
</set>
<set name="nodes_Input">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="remove">
<atom>Three</atom>
</set>
<set name="root_Input">
<atom>One</atom>
</set>
<relation name="child_Input">
<map>
<atom>One</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
<relation name="color_Input">
<map>
<atom>Two</atom>
<range>
<atom>Red</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Red</atom>
</range>
</map>
</relation>
<relation name="left_Input">
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
<relation name="parent_Input">
<map>
<atom>Two</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Zero</atom>
</range>
</map>
</relation>
<relation name="right_Input">
<map>
<atom>One</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
</instance>



<instance name="state">
<domain name="Color">
<atom>Red</atom>
<atom>Black</atom>
</domain>
<domain name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</domain>
<set name="Color">
<atom>Red</atom>
<atom>Black</atom>
</set>
<set name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="Black">
<atom>Black</atom>
</set>
<set name="NIL">
<atom>NIL</atom>
</set>
<set name="One">
<atom>One</atom>
</set>
<set name="Red">
<atom>Red</atom>
</set>
<set name="Three">
<atom>Three</atom>
</set>
<set name="Two">
<atom>Two</atom>
</set>
<set name="Zero">
<atom>Zero</atom>
</set>
<set name="nodes_Input">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Three</atom>
</set>
<set name="remove">
<atom>Three</atom>
</set>
<set name="root_Input">
<atom>One</atom>
</set>
<relation name="child_Input">
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
<relation name="color_Input">
<map>
<atom>Three</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Red</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>Black</atom>
</range>
</map>
</relation>
<relation name="left_Input">
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
<relation name="parent_Input">
<map>
<atom>NIL</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>One</atom>
</range>
</map>
</relation>
<relation name="right_Input">
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Three</atom>
</range>
</map>
</relation>
</instance>



<instance name="state">
<domain name="Color">
<atom>Red</atom>
<atom>Black</atom>
</domain>
<domain name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</domain>
<set name="Color">
<atom>Red</atom>
<atom>Black</atom>
</set>
<set name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="Black">
<atom>Black</atom>
</set>
<set name="NIL">
<atom>NIL</atom>
</set>
<set name="One">
<atom>One</atom>
</set>
<set name="Red">
<atom>Red</atom>
</set>
<set name="Three">
<atom>Three</atom>
</set>
<set name="Two">
<atom>Two</atom>
</set>
<set name="Zero">
<atom>Zero</atom>
</set>
<set name="nodes_Input">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Three</atom>
</set>
<set name="remove">
<atom>One</atom>
</set>
<set name="root_Input">
<atom>One</atom>
</set>
<relation name="child_Input">
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Three</atom>
</range>
</map>
</relation>
<relation name="color_Input">
<map>
<atom>NIL</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Red</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>Black</atom>
</range>
</map>
</relation>
<relation name="left_Input">
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
</relation>
<relation name="parent_Input">
<map>
<atom>Three</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Three</atom>
</range>
</map>
</relation>
<relation name="right_Input">
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Three</atom>
</range>
</map>
</relation>
</instance>



<instance name="state">
<domain name="Color">
<atom>Red</atom>
<atom>Black</atom>
</domain>
<domain name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</domain>
<set name="Color">
<atom>Red</atom>
<atom>Black</atom>
</set>
<set name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="Black">
<atom>Black</atom>
</set>
<set name="NIL">
<atom>NIL</atom>
</set>
<set name="One">
<atom>One</atom>
</set>
<set name="Red">
<atom>Red</atom>
</set>
<set name="Three">
<atom>Three</atom>
</set>
<set name="Two">
<atom>Two</atom>
</set>
<set name="Zero">
<atom>Zero</atom>
</set>
<set name="nodes_Input">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Three</atom>
</set>
<set name="remove">
<atom>Zero</atom>
</set>
<set name="root_Input">
<atom>One</atom>
</set>
<relation name="child_Input">
<map>
<atom>One</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
<relation name="color_Input">
<map>
<atom>NIL</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Red</atom>
</range>
</map>
</relation>
<relation name="left_Input">
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
</relation>
<relation name="parent_Input">
<map>
<atom>Three</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>One</atom>
</range>
</map>
</relation>
<relation name="right_Input">
<map>
<atom>One</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
</instance>



<instance name="state">
<domain name="Color">
<atom>Red</atom>
<atom>Black</atom>
</domain>
<domain name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</domain>
<set name="Color">
<atom>Red</atom>
<atom>Black</atom>
</set>
<set name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="Black">
<atom>Black</atom>
</set>
<set name="NIL">
<atom>NIL</atom>
</set>
<set name="One">
<atom>One</atom>
</set>
<set name="Red">
<atom>Red</atom>
</set>
<set name="Three">
<atom>Three</atom>
</set>
<set name="Two">
<atom>Two</atom>
</set>
<set name="Zero">
<atom>Zero</atom>
</set>
<set name="nodes_Input">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="remove">
<atom>Zero</atom>
</set>
<set name="root_Input">
<atom>One</atom>
</set>
<relation name="child_Input">
<map>
<atom>One</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
<relation name="color_Input">
<map>
<atom>Three</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Red</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Black</atom>
</range>
</map>
</relation>
<relation name="left_Input">
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
<relation name="parent_Input">
<map>
<atom>Zero</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Three</atom>
</range>
</map>
</relation>
<relation name="right_Input">
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
</instance>



<instance name="state">
<domain name="Color">
<atom>Red</atom>
<atom>Black</atom>
</domain>
<domain name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</domain>
<set name="Color">
<atom>Red</atom>
<atom>Black</atom>
</set>
<set name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="Black">
<atom>Black</atom>
</set>
<set name="NIL">
<atom>NIL</atom>
</set>
<set name="One">
<atom>One</atom>
</set>
<set name="Red">
<atom>Red</atom>
</set>
<set name="Three">
<atom>Three</atom>
</set>
<set name="Two">
<atom>Two</atom>
</set>
<set name="Zero">
<atom>Zero</atom>
</set>
<set name="nodes_Input">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="remove">
<atom>One</atom>
</set>
<set name="root_Input">
<atom>One</atom>
</set>
<relation name="child_Input">
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
<relation name="color_Input">
<map>
<atom>Three</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Red</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>Black</atom>
</range>
</map>
</relation>
<relation name="left_Input">
<map>
<atom>Two</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
<relation name="parent_Input">
<map>
<atom>Three</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>One</atom>
</range>
</map>
</relation>
<relation name="right_Input">
<map>
<atom>One</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
</instance>



<instance name="state">
<domain name="Color">
<atom>Red</atom>
<atom>Black</atom>
</domain>
<domain name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</domain>
<set name="Color">
<atom>Red</atom>
<atom>Black</atom>
</set>
<set name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="Black">
<atom>Black</atom>
</set>
<set name="NIL">
<atom>NIL</atom>
</set>
<set name="One">
<atom>One</atom>
</set>
<set name="Red">
<atom>Red</atom>
</set>
<set name="Three">
<atom>Three</atom>
</set>
<set name="Two">
<atom>Two</atom>
</set>
<set name="Zero">
<atom>Zero</atom>
</set>
<set name="nodes_Input">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="remove">
<atom>Two</atom>
</set>
<set name="root_Input">
<atom>One</atom>
</set>
<relation name="child_Input">
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
</relation>
<relation name="color_Input">
<map>
<atom>Three</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Red</atom>
</range>
</map>
</relation>
<relation name="left_Input">
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>Two</atom>
</range>
</map>
</relation>
<relation name="parent_Input">
<map>
<atom>NIL</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Two</atom>
</range>
</map>
</relation>
<relation name="right_Input">
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
</instance>



<instance name="state">
<domain name="Color">
<atom>Red</atom>
<atom>Black</atom>
</domain>
<domain name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</domain>
<set name="Color">
<atom>Red</atom>
<atom>Black</atom>
</set>
<set name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="Black">
<atom>Black</atom>
</set>
<set name="NIL">
<atom>NIL</atom>
</set>
<set name="One">
<atom>One</atom>
</set>
<set name="Red">
<atom>Red</atom>
</set>
<set name="Three">
<atom>Three</atom>
</set>
<set name="Two">
<atom>Two</atom>
</set>
<set name="Zero">
<atom>Zero</atom>
</set>
<set name="nodes_Input">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="remove">
<atom>Three</atom>
</set>
<set name="root_Input">
<atom>One</atom>
</set>
<relation name="child_Input">
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
<relation name="color_Input">
<map>
<atom>Zero</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Red</atom>
</range>
</map>
</relation>
<relation name="left_Input">
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>Two</atom>
</range>
</map>
</relation>
<relation name="parent_Input">
<map>
<atom>NIL</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Two</atom>
</range>
</map>
</relation>
<relation name="right_Input">
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
</instance>



<instance name="state">
<domain name="Color">
<atom>Red</atom>
<atom>Black</atom>
</domain>
<domain name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</domain>
<set name="Color">
<atom>Red</atom>
<atom>Black</atom>
</set>
<set name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="Black">
<atom>Black</atom>
</set>
<set name="NIL">
<atom>NIL</atom>
</set>
<set name="One">
<atom>One</atom>
</set>
<set name="Red">
<atom>Red</atom>
</set>
<set name="Three">
<atom>Three</atom>
</set>
<set name="Two">
<atom>Two</atom>
</set>
<set name="Zero">
<atom>Zero</atom>
</set>
<set name="nodes_Input">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Three</atom>
</set>
<set name="remove">
<atom>Three</atom>
</set>
<set name="root_Input">
<atom>One</atom>
</set>
<relation name="child_Input">
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Three</atom>
</range>
</map>
</relation>
<relation name="color_Input">
<map>
<atom>Three</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>Black</atom>
</range>
</map>
</relation>
<relation name="left_Input">
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
<relation name="parent_Input">
<map>
<atom>Three</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Three</atom>
</range>
</map>
</relation>
<relation name="right_Input">
<map>
<atom>One</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
</instance>



<instance name="state">
<domain name="Color">
<atom>Red</atom>
<atom>Black</atom>
</domain>
<domain name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</domain>
<set name="Color">
<atom>Red</atom>
<atom>Black</atom>
</set>
<set name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="Black">
<atom>Black</atom>
</set>
<set name="NIL">
<atom>NIL</atom>
</set>
<set name="One">
<atom>One</atom>
</set>
<set name="Red">
<atom>Red</atom>
</set>
<set name="Three">
<atom>Three</atom>
</set>
<set name="Two">
<atom>Two</atom>
</set>
<set name="Zero">
<atom>Zero</atom>
</set>
<set name="nodes_Input">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Three</atom>
</set>
<set name="remove">
<atom>One</atom>
</set>
<set name="root_Input">
<atom>One</atom>
</set>
<relation name="child_Input">
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
</relation>
<relation name="color_Input">
<map>
<atom>Three</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Black</atom>
</range>
</map>
</relation>
<relation name="left_Input">
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
<relation name="parent_Input">
<map>
<atom>Three</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>One</atom>
</range>
</map>
</relation>
<relation name="right_Input">
<map>
<atom>One</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
</instance>



<instance name="state">
<domain name="Color">
<atom>Red</atom>
<atom>Black</atom>
</domain>
<domain name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</domain>
<set name="Color">
<atom>Red</atom>
<atom>Black</atom>
</set>
<set name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="Black">
<atom>Black</atom>
</set>
<set name="NIL">
<atom>NIL</atom>
</set>
<set name="One">
<atom>One</atom>
</set>
<set name="Red">
<atom>Red</atom>
</set>
<set name="Three">
<atom>Three</atom>
</set>
<set name="Two">
<atom>Two</atom>
</set>
<set name="Zero">
<atom>Zero</atom>
</set>
<set name="nodes_Input">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Three</atom>
</set>
<set name="remove">
<atom>Zero</atom>
</set>
<set name="root_Input">
<atom>One</atom>
</set>
<relation name="child_Input">
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Three</atom>
</range>
</map>
</relation>
<relation name="color_Input">
<map>
<atom>Three</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Black</atom>
</range>
</map>
</relation>
<relation name="left_Input">
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
<relation name="parent_Input">
<map>
<atom>Three</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Three</atom>
</range>
</map>
</relation>
<relation name="right_Input">
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Three</atom>
</range>
</map>
</relation>
</instance>



<instance name="state">
<domain name="Color">
<atom>Red</atom>
<atom>Black</atom>
</domain>
<domain name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</domain>
<set name="Color">
<atom>Red</atom>
<atom>Black</atom>
</set>
<set name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="Black">
<atom>Black</atom>
</set>
<set name="NIL">
<atom>NIL</atom>
</set>
<set name="One">
<atom>One</atom>
</set>
<set name="Red">
<atom>Red</atom>
</set>
<set name="Three">
<atom>Three</atom>
</set>
<set name="Two">
<atom>Two</atom>
</set>
<set name="Zero">
<atom>Zero</atom>
</set>
<set name="nodes_Input">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Three</atom>
</set>
<set name="remove">
<atom>Three</atom>
</set>
<set name="root_Input">
<atom>One</atom>
</set>
<relation name="child_Input">
<map>
<atom>One</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
<relation name="color_Input">
<map>
<atom>Three</atom>
<range>
<atom>Red</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>Red</atom>
</range>
</map>
</relation>
<relation name="left_Input">
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
</relation>
<relation name="parent_Input">
<map>
<atom>Three</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Zero</atom>
</range>
</map>
</relation>
<relation name="right_Input">
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
</instance>



<instance name="state">
<domain name="Color">
<atom>Red</atom>
<atom>Black</atom>
</domain>
<domain name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</domain>
<set name="Color">
<atom>Red</atom>
<atom>Black</atom>
</set>
<set name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="Black">
<atom>Black</atom>
</set>
<set name="NIL">
<atom>NIL</atom>
</set>
<set name="One">
<atom>One</atom>
</set>
<set name="Red">
<atom>Red</atom>
</set>
<set name="Three">
<atom>Three</atom>
</set>
<set name="Two">
<atom>Two</atom>
</set>
<set name="Zero">
<atom>Zero</atom>
</set>
<set name="nodes_Input">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Three</atom>
</set>
<set name="remove">
<atom>One</atom>
</set>
<set name="root_Input">
<atom>One</atom>
</set>
<relation name="child_Input">
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Three</atom>
</range>
</map>
</relation>
<relation name="color_Input">
<map>
<atom>Three</atom>
<range>
<atom>Red</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>Red</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Black</atom>
</range>
</map>
</relation>
<relation name="left_Input">
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
<relation name="parent_Input">
<map>
<atom>Three</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Three</atom>
</range>
</map>
</relation>
<relation name="right_Input">
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Three</atom>
</range>
</map>
</relation>
</instance>



<instance name="state">
<domain name="Color">
<atom>Red</atom>
<atom>Black</atom>
</domain>
<domain name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</domain>
<set name="Color">
<atom>Red</atom>
<atom>Black</atom>
</set>
<set name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="Black">
<atom>Black</atom>
</set>
<set name="NIL">
<atom>NIL</atom>
</set>
<set name="One">
<atom>One</atom>
</set>
<set name="Red">
<atom>Red</atom>
</set>
<set name="Three">
<atom>Three</atom>
</set>
<set name="Two">
<atom>Two</atom>
</set>
<set name="Zero">
<atom>Zero</atom>
</set>
<set name="nodes_Input">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Three</atom>
</set>
<set name="remove">
<atom>Zero</atom>
</set>
<set name="root_Input">
<atom>One</atom>
</set>
<relation name="child_Input">
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
</relation>
<relation name="color_Input">
<map>
<atom>One</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>Red</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>Red</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Black</atom>
</range>
</map>
</relation>
<relation name="left_Input">
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
</relation>
<relation name="parent_Input">
<map>
<atom>Zero</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>Three</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Three</atom>
</range>
</map>
</relation>
<relation name="right_Input">
<map>
<atom>Three</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Three</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
</instance>



<instance name="state">
<domain name="Color">
<atom>Red</atom>
<atom>Black</atom>
</domain>
<domain name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</domain>
<set name="Color">
<atom>Red</atom>
<atom>Black</atom>
</set>
<set name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="Black">
<atom>Black</atom>
</set>
<set name="NIL">
<atom>NIL</atom>
</set>
<set name="One">
<atom>One</atom>
</set>
<set name="Red">
<atom>Red</atom>
</set>
<set name="Three">
<atom>Three</atom>
</set>
<set name="Two">
<atom>Two</atom>
</set>
<set name="Zero">
<atom>Zero</atom>
</set>
<set name="nodes_Input">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
</set>
<set name="remove">
<atom>Zero</atom>
</set>
<set name="root_Input">
<atom>One</atom>
</set>
<relation name="child_Input">
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
</relation>
<relation name="color_Input">
<map>
<atom>Zero</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Black</atom>
</range>
</map>
</relation>
<relation name="left_Input">
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
<relation name="parent_Input">
<map>
<atom>NIL</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>One</atom>
</range>
</map>
</relation>
<relation name="right_Input">
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
</instance>



<instance name="state">
<domain name="Color">
<atom>Red</atom>
<atom>Black</atom>
</domain>
<domain name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</domain>
<set name="Color">
<atom>Red</atom>
<atom>Black</atom>
</set>
<set name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="Black">
<atom>Black</atom>
</set>
<set name="NIL">
<atom>NIL</atom>
</set>
<set name="One">
<atom>One</atom>
</set>
<set name="Red">
<atom>Red</atom>
</set>
<set name="Three">
<atom>Three</atom>
</set>
<set name="Two">
<atom>Two</atom>
</set>
<set name="Zero">
<atom>Zero</atom>
</set>
<set name="nodes_Input">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
</set>
<set name="remove">
<atom>One</atom>
</set>
<set name="root_Input">
<atom>One</atom>
</set>
<relation name="child_Input">
<map>
<atom>Two</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
<relation name="color_Input">
<map>
<atom>NIL</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>Black</atom>
</range>
</map>
</relation>
<relation name="left_Input">
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
<relation name="parent_Input">
<map>
<atom>Zero</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Two</atom>
</range>
</map>
</relation>
<relation name="right_Input">
<map>
<atom>Two</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Two</atom>
</range>
</map>
</relation>
</instance>



<instance name="state">
<domain name="Color">
<atom>Red</atom>
<atom>Black</atom>
</domain>
<domain name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</domain>
<set name="Color">
<atom>Red</atom>
<atom>Black</atom>
</set>
<set name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="Black">
<atom>Black</atom>
</set>
<set name="NIL">
<atom>NIL</atom>
</set>
<set name="One">
<atom>One</atom>
</set>
<set name="Red">
<atom>Red</atom>
</set>
<set name="Three">
<atom>Three</atom>
</set>
<set name="Two">
<atom>Two</atom>
</set>
<set name="Zero">
<atom>Zero</atom>
</set>
<set name="nodes_Input">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
</set>
<set name="remove">
<atom>Two</atom>
</set>
<set name="root_Input">
<atom>One</atom>
</set>
<relation name="child_Input">
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Two</atom>
</range>
</map>
</relation>
<relation name="color_Input">
<map>
<atom>Zero</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Black</atom>
</range>
</map>
</relation>
<relation name="left_Input">
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
<relation name="parent_Input">
<map>
<atom>Two</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Two</atom>
</range>
</map>
</relation>
<relation name="right_Input">
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Two</atom>
</range>
</map>
</relation>
</instance>



<instance name="state">
<domain name="Color">
<atom>Red</atom>
<atom>Black</atom>
</domain>
<domain name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</domain>
<set name="Color">
<atom>Red</atom>
<atom>Black</atom>
</set>
<set name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="Black">
<atom>Black</atom>
</set>
<set name="NIL">
<atom>NIL</atom>
</set>
<set name="One">
<atom>One</atom>
</set>
<set name="Red">
<atom>Red</atom>
</set>
<set name="Three">
<atom>Three</atom>
</set>
<set name="Two">
<atom>Two</atom>
</set>
<set name="Zero">
<atom>Zero</atom>
</set>
<set name="nodes_Input">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
</set>
<set name="remove">
<atom>Zero</atom>
</set>
<set name="root_Input">
<atom>Zero</atom>
</set>
<relation name="child_Input">
<map>
<atom>One</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>One</atom>
</range>
</map>
</relation>
<relation name="color_Input">
<map>
<atom>NIL</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Red</atom>
</range>
</map>
</relation>
<relation name="left_Input">
<map>
<atom>One</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
<relation name="parent_Input">
<map>
<atom>NIL</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Zero</atom>
</range>
</map>
</relation>
<relation name="right_Input">
<map>
<atom>One</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>One</atom>
</range>
</map>
</relation>
</instance>



<instance name="state">
<domain name="Color">
<atom>Red</atom>
<atom>Black</atom>
</domain>
<domain name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</domain>
<set name="Color">
<atom>Red</atom>
<atom>Black</atom>
</set>
<set name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="Black">
<atom>Black</atom>
</set>
<set name="NIL">
<atom>NIL</atom>
</set>
<set name="One">
<atom>One</atom>
</set>
<set name="Red">
<atom>Red</atom>
</set>
<set name="Three">
<atom>Three</atom>
</set>
<set name="Two">
<atom>Two</atom>
</set>
<set name="Zero">
<atom>Zero</atom>
</set>
<set name="nodes_Input">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
</set>
<set name="remove">
<atom>One</atom>
</set>
<set name="root_Input">
<atom>Zero</atom>
</set>
<relation name="child_Input">
<map>
<atom>One</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>One</atom>
</range>
</map>
</relation>
<relation name="color_Input">
<map>
<atom>One</atom>
<range>
<atom>Red</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Black</atom>
</range>
</map>
</relation>
<relation name="left_Input">
<map>
<atom>One</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
<relation name="parent_Input">
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Zero</atom>
</range>
</map>
</relation>
<relation name="right_Input">
<map>
<atom>One</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>One</atom>
</range>
</map>
</relation>
</instance>



<instance name="state">
<domain name="Color">
<atom>Red</atom>
<atom>Black</atom>
</domain>
<domain name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</domain>
<set name="Color">
<atom>Red</atom>
<atom>Black</atom>
</set>
<set name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="Black">
<atom>Black</atom>
</set>
<set name="NIL">
<atom>NIL</atom>
</set>
<set name="One">
<atom>One</atom>
</set>
<set name="Red">
<atom>Red</atom>
</set>
<set name="Three">
<atom>Three</atom>
</set>
<set name="Two">
<atom>Two</atom>
</set>
<set name="Zero">
<atom>Zero</atom>
</set>
<set name="nodes_Input">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
</set>
<set name="remove">
<atom>Two</atom>
</set>
<set name="root_Input">
<atom>One</atom>
</set>
<relation name="child_Input">
<map>
<atom>One</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
<relation name="color_Input">
<map>
<atom>One</atom>
<range>
<atom>Red</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Black</atom>
</range>
</map>
</relation>
<relation name="left_Input">
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
<relation name="parent_Input">
<map>
<atom>NIL</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Two</atom>
</range>
</map>
</relation>
<relation name="right_Input">
<map>
<atom>One</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
</instance>



<instance name="state">
<domain name="Color">
<atom>Red</atom>
<atom>Black</atom>
</domain>
<domain name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</domain>
<set name="Color">
<atom>Red</atom>
<atom>Black</atom>
</set>
<set name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="Black">
<atom>Black</atom>
</set>
<set name="NIL">
<atom>NIL</atom>
</set>
<set name="One">
<atom>One</atom>
</set>
<set name="Red">
<atom>Red</atom>
</set>
<set name="Three">
<atom>Three</atom>
</set>
<set name="Two">
<atom>Two</atom>
</set>
<set name="Zero">
<atom>Zero</atom>
</set>
<set name="nodes_Input">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
</set>
<set name="remove">
<atom>One</atom>
</set>
<set name="root_Input">
<atom>One</atom>
</set>
<relation name="child_Input">
<map>
<atom>Two</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
<relation name="color_Input">
<map>
<atom>One</atom>
<range>
<atom>Red</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Black</atom>
</range>
</map>
</relation>
<relation name="left_Input">
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
<relation name="parent_Input">
<map>
<atom>Two</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Zero</atom>
</range>
</map>
</relation>
<relation name="right_Input">
<map>
<atom>One</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
</instance>



<instance name="state">
<domain name="Color">
<atom>Red</atom>
<atom>Black</atom>
</domain>
<domain name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</domain>
<set name="Color">
<atom>Red</atom>
<atom>Black</atom>
</set>
<set name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="Black">
<atom>Black</atom>
</set>
<set name="NIL">
<atom>NIL</atom>
</set>
<set name="One">
<atom>One</atom>
</set>
<set name="Red">
<atom>Red</atom>
</set>
<set name="Three">
<atom>Three</atom>
</set>
<set name="Two">
<atom>Two</atom>
</set>
<set name="Zero">
<atom>Zero</atom>
</set>
<set name="nodes_Input">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
</set>
<set name="remove">
<atom>Zero</atom>
</set>
<set name="root_Input">
<atom>One</atom>
</set>
<relation name="child_Input">
<map>
<atom>Two</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
</relation>
<relation name="color_Input">
<map>
<atom>Zero</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Red</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Black</atom>
</range>
</map>
</relation>
<relation name="left_Input">
<map>
<atom>Two</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
</relation>
<relation name="parent_Input">
<map>
<atom>Zero</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>One</atom>
</range>
</map>
</relation>
<relation name="right_Input">
<map>
<atom>Two</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
</instance>



<instance name="state">
<domain name="Color">
<atom>Red</atom>
<atom>Black</atom>
</domain>
<domain name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</domain>
<set name="Color">
<atom>Red</atom>
<atom>Black</atom>
</set>
<set name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="Black">
<atom>Black</atom>
</set>
<set name="NIL">
<atom>NIL</atom>
</set>
<set name="One">
<atom>One</atom>
</set>
<set name="Red">
<atom>Red</atom>
</set>
<set name="Three">
<atom>Three</atom>
</set>
<set name="Two">
<atom>Two</atom>
</set>
<set name="Zero">
<atom>Zero</atom>
</set>
<set name="nodes_Input">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
</set>
<set name="remove">
<atom>One</atom>
</set>
<set name="root_Input">
<atom>One</atom>
</set>
<relation name="child_Input">
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
<relation name="color_Input">
<map>
<atom>One</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>Red</atom>
</range>
</map>
</relation>
<relation name="left_Input">
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
</relation>
<relation name="parent_Input">
<map>
<atom>Zero</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Zero</atom>
</range>
</map>
</relation>
<relation name="right_Input">
<map>
<atom>One</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
</instance>



<instance name="state">
<domain name="Color">
<atom>Red</atom>
<atom>Black</atom>
</domain>
<domain name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</domain>
<set name="Color">
<atom>Red</atom>
<atom>Black</atom>
</set>
<set name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="Black">
<atom>Black</atom>
</set>
<set name="NIL">
<atom>NIL</atom>
</set>
<set name="One">
<atom>One</atom>
</set>
<set name="Red">
<atom>Red</atom>
</set>
<set name="Three">
<atom>Three</atom>
</set>
<set name="Two">
<atom>Two</atom>
</set>
<set name="Zero">
<atom>Zero</atom>
</set>
<set name="nodes_Input">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
</set>
<set name="remove">
<atom>Zero</atom>
</set>
<set name="root_Input">
<atom>One</atom>
</set>
<relation name="child_Input">
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
<relation name="color_Input">
<map>
<atom>Zero</atom>
<range>
<atom>Red</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Black</atom>
</range>
</map>
</relation>
<relation name="left_Input">
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
<relation name="parent_Input">
<map>
<atom>NIL</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Zero</atom>
</range>
</map>
</relation>
<relation name="right_Input">
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
</instance>



<instance name="state">
<domain name="Color">
<atom>Red</atom>
<atom>Black</atom>
</domain>
<domain name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</domain>
<set name="Color">
<atom>Red</atom>
<atom>Black</atom>
</set>
<set name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="Black">
<atom>Black</atom>
</set>
<set name="NIL">
<atom>NIL</atom>
</set>
<set name="One">
<atom>One</atom>
</set>
<set name="Red">
<atom>Red</atom>
</set>
<set name="Three">
<atom>Three</atom>
</set>
<set name="Two">
<atom>Two</atom>
</set>
<set name="Zero">
<atom>Zero</atom>
</set>
<set name="nodes_Input">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
</set>
<set name="remove">
<atom>Two</atom>
</set>
<set name="root_Input">
<atom>One</atom>
</set>
<relation name="child_Input">
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Two</atom>
</range>
</map>
</relation>
<relation name="color_Input">
<map>
<atom>One</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Red</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>Red</atom>
</range>
</map>
</relation>
<relation name="left_Input">
<map>
<atom>Two</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
<relation name="parent_Input">
<map>
<atom>Two</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Zero</atom>
</range>
</map>
</relation>
<relation name="right_Input">
<map>
<atom>Two</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
</instance>



<instance name="state">
<domain name="Color">
<atom>Red</atom>
<atom>Black</atom>
</domain>
<domain name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</domain>
<set name="Color">
<atom>Red</atom>
<atom>Black</atom>
</set>
<set name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="Black">
<atom>Black</atom>
</set>
<set name="NIL">
<atom>NIL</atom>
</set>
<set name="One">
<atom>One</atom>
</set>
<set name="Red">
<atom>Red</atom>
</set>
<set name="Three">
<atom>Three</atom>
</set>
<set name="Two">
<atom>Two</atom>
</set>
<set name="Zero">
<atom>Zero</atom>
</set>
<set name="nodes_Input">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
</set>
<set name="remove">
<atom>Zero</atom>
</set>
<set name="root_Input">
<atom>One</atom>
</set>
<relation name="child_Input">
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Two</atom>
</range>
</map>
</relation>
<relation name="color_Input">
<map>
<atom>Zero</atom>
<range>
<atom>Red</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Red</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Black</atom>
</range>
</map>
</relation>
<relation name="left_Input">
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
<relation name="parent_Input">
<map>
<atom>Two</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>One</atom>
</range>
</map>
</relation>
<relation name="right_Input">
<map>
<atom>Two</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
</instance>



<instance name="state">
<domain name="Color">
<atom>Red</atom>
<atom>Black</atom>
</domain>
<domain name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</domain>
<set name="Color">
<atom>Red</atom>
<atom>Black</atom>
</set>
<set name="Node">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
<atom>Three</atom>
</set>
<set name="Black">
<atom>Black</atom>
</set>
<set name="NIL">
<atom>NIL</atom>
</set>
<set name="One">
<atom>One</atom>
</set>
<set name="Red">
<atom>Red</atom>
</set>
<set name="Three">
<atom>Three</atom>
</set>
<set name="Two">
<atom>Two</atom>
</set>
<set name="Zero">
<atom>Zero</atom>
</set>
<set name="nodes_Input">
<atom>NIL</atom>
<atom>Zero</atom>
<atom>One</atom>
<atom>Two</atom>
</set>
<set name="remove">
<atom>One</atom>
</set>
<set name="root_Input">
<atom>One</atom>
</set>
<relation name="child_Input">
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
<relation name="color_Input">
<map>
<atom>One</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Black</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>Red</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>Red</atom>
</range>
</map>
</relation>
<relation name="left_Input">
<map>
<atom>One</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
<relation name="parent_Input">
<map>
<atom>NIL</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>NIL</atom>
<range>
<atom>Zero</atom>
</range>
</map>
<map>
<atom>Two</atom>
<range>
<atom>One</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>One</atom>
</range>
</map>
</relation>
<relation name="right_Input">
<map>
<atom>Two</atom>
<range>
<atom>NIL</atom>
</range>
</map>
<map>
<atom>One</atom>
<range>
<atom>Two</atom>
</range>
</map>
<map>
<atom>Zero</atom>
<range>
<atom>NIL</atom>
</range>
</map>
</relation>
</instance>


</instlist>

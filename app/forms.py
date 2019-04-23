"""
Definition of forms.
"""

from django import forms
from django.contrib.auth.forms import AuthenticationForm
from django.utils.translation import ugettext_lazy as _


class BootstrapAuthenticationForm(AuthenticationForm):
    """Authentication form which uses boostrap CSS."""

    username = forms.CharField(max_length=254,
                               widget=forms.TextInput({
                                   'class': 'form-control',
                                   'placeholder': 'Email'}))
    password = forms.CharField(label=_("Password"),
                               widget=forms.PasswordInput({
                                   'class': 'form-control',
                                   'placeholder': 'Password'}))
    # firstName = forms.CharField(max_length=254,
    #                           widget=forms.TextInput({
    #                               'class': 'form-control',
    #                               'placeholder': 'First Name'}))
    # lastName = forms.CharField(max_length=254,
    #                           widget=forms.TextInput({
    #                               'class': 'form-control',
    #                               'placeholder': 'Last Name'}))
    # occupation = forms.CharField(max_length=254,
    #                           widget=forms.TextInput({
    #                               'class': 'form-control',
    #                               'placeholder': 'Occupation'}))
    # company = forms.CharField(max_length=254,
    #                           widget=forms.TextInput({
    #                               'class': 'form-control',
    #                               'placeholder': 'Company'}))
    # email = forms.CharField(max_length=254,
    #                        widget=forms.TextInput({
    #                            'class': 'form-control',
    #                            'placeholder': 'Email/Username'}))
    # password = forms.CharField(label=_("Password"),
    #                           widget=forms.PasswordInput({
    #                               'class': 'form-control',
    #                               'placeholder':'Password'}))


class DisplayWeatherDataForm(forms.Form):
    state = forms.Select()
    city = forms.Select()
    date = forms.DateField()

class CreateInitFileForm(forms.Form):
    sim_title = forms.Select()
    lake_name = forms.Select()
    state = forms.Select()
    start_date_timepicker = forms.Select()
    end_date_timepicker = forms.Select()
    num_horiz_layers = forms.Select()
    max_depth = forms.Select()
    elevation = forms.Select()
    light_atten_water = forms.Select()
    light_atten_chlor = forms.Select()
    xk1 = forms.Select()
    temp_wind_sheltering = forms.Select()
    wind_sheltering_summer = forms.Select()
    wind_sheltering_fall = forms.Select()
    snow_ice_ratio = forms.Select()
    std = forms.Select()
    sediment_density = forms.Select()
    snow_compact = forms.Select()
    ice_conduct = forms.Select()
    snow_conduct = forms.Select()
    wcht_coefficient = forms.Select()
    max_snow_ice_thickness_ratio = forms.Select()
    ice_abs_coefficient = forms.Select()
    ice_reflect_coefficient = forms.Select()
    ice_attn_coefficient = forms.Select()
    snow_abs_coefficient = forms.Select()
    snow_reflect_coefficient = forms.Select()
    snow_attn_coefficient = forms.Select()
    init_ice_thickness = forms.Select()
    init_snow_thickness = forms.Select()
    init_water_depths_div = forms.Select()
    init_water_temps = forms.Select()
    init_sus_solids_conc = forms.Select()
    total_diss_solids = forms.Select()
    init_phos_conc = forms.Select()
    init_diss_oxy = forms.Select()
    det_decay_rate = forms.Select()
    alg_resp_rater = forms.Select()
    max_photo_ratio = forms.Select()
    is_outflow_file = forms.Select()
    is_plot_file = forms.Select()
    num_depth_plots = forms.Select()
    tab_data_interval = forms.Select()
    inflow_outflow_source = forms.Select()
    time_series_output = forms.Select()
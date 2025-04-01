
% Eventuality types and thematic roles

ct_Eventuality(soaPay).

ct_ThematicRole(soaHas_agent).
ct_ThematicRole(soaHas_object).
ct_ThematicRole(soaHas_instrument).


% Input facts
ct_triple(soa_enpcj, type, obligatory).
ct_triple(soa_enpcj, not, soa_epcj).

ct_triple(soa_epcj, type, soaPay).
ct_triple(soa_epcj, soaHas_agent, soa_John).
ct_triple(soa_epcj, soaHas_instrument, soa_cash).

ct_triple(soa_ep3cj, type, obligatory).
ct_triple(soa_ep3cj, type, soaPay).
ct_triple(soa_ep3cj, soaHas_agent, soa_John).
ct_triple(soa_ep3cj, soaHas_object, amount(3, pounds)).
ct_triple(soa_ep3cj, soaHas_instrument, soa_cash).

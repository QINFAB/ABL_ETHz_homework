clear;
clc;
%% preliminaries (define constatnts)
g = 9.81; % acceleartion due to gravity (m s-2)
vK = 0.4; % von Karman constant
%% read the data
root_path = 'data/'; % Please indicate here the folder where you placed the data, with the full Path if necessary.
filename = 'SurfaceData_Cabauw_05-10-May-2008.txt';
filepath = fullfile(root_path, filename);
txtcon = readtable(filepath);
data = table2array(txtcon);
%% define relevant variables 
z2 = 2;
z5 = 5;
z10 = 10;

yy = data(:,1); 
mm = data(:,2);
dd = data(:,3);
hhmin = data(:,4);
Ta002 = data(:,5) + 273.15;
U010 = data(:,6);
ust005 = data(:,7);
wT005 = data(:,8);

% hh = floor(hhmin); % floor: take the integer part as the hour
% min = (hhmin - hh)*60;
% timepoint = datetime(yy, mm, dd, hh, min, 0); % convert the time into the "datetime" format

% In this case, we only keep the hour and minute information (hhmin), since only
% diurnal variation is needed to show
%% Evaluate requested quantities
L = -ust005.^3./(vK.*9.81./Ta002.*wT005);
zol = z5/L;
%% plot results
color = [0.2, 0.5, 0.7];
shape = 'o';
period = [0, 24]; % define the start (0 hr) and end (24 hr) of period

% plot the datapoints
scatter(hhmin, L, 24, color, shape, 'filled');
hold on;
grid on;
% define the limit of x and y axis
xlim([period(1), period(2)]);
ylim([-150, 200]);
% plot the horizontal and vertical lines
yline(0, '--', 'LineWidth', 0.75, 'Color', 'k');
xline(6, '-', 'LineWidth', 0.75, 'Color', 'k');
xline(16, '-', 'LineWidth', 0.75, 'Color', 'k');
% plot the vertical lines of sunrise and sunset
sunrise_time = 4;
sunset_time = 19;
xline(sunrise_time, '--', 'LineWidth', 0.75, 'Color', 'k');
xline(sunset_time, '--', 'LineWidth', 0.75, 'Color', 'k');
% Label the vertical lines with "sunrise" and "sunset"
text(sunrise_time, -120, 'Sunrise', 'HorizontalAlignment', 'center', 'VerticalAlignment', 'bottom', 'FontSize', 10, 'Color', 'k', 'Rotation', 90);
text(sunset_time, -120, 'Sunset', 'HorizontalAlignment', 'center', 'VerticalAlignment', 'bottom', 'FontSize', 10, 'Color', 'k', 'Rotation', 90);

set(gca,'XTick',[period(1):6:period(2)],'XTickLabel', {'0', '6', '12', '18', '24'}, 'fontsize',10);  
set(gca,'YTick',[-150:50:200],'fontsize',10); 

xlabel('Hour', 'FontSize', 10, 'FontName', 'Arial');
ylabel('L (m)', 'FontSize', 10, 'FontName', 'Arial');